{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Run where

import App.Config
import App.Exceptions
import App.Tables
import App.Types
import Control.Lens hiding (elements)
import Data.Aeson (ToJSON (..), Value (..))
import Data.Aeson.Lens (key, _Array, _Bool, _String)
import Data.Attoparsec.ByteString (parseOnly)
import qualified Data.ByteString as BS (writeFile)
import Data.Monoid (First)
import qualified Data.Text.Encoding as T (decodeUtf8)
import qualified Data.Text.IO as T (getLine, writeFile)
import qualified Data.Text.Internal.Search as T (indices)
import qualified Data.Text.Lazy.IO as TL (writeFile)
import Data.Tuple (swap)
import Database.Esqueleto.Experimental
    ( Entity
    , PersistEntity
    , PersistField
    , SqlExpr
    , SqlQuery
    , from
    , runSqlConn
    , select
    , transactionSave
    , transactionUndo
    , update
    , val
    , where_
    , (=.)
    , (==.)
    )
import qualified Database.Esqueleto.Experimental as ES
import Formatting (Format (..), format, (%))
import Formatting.Combinators (lpadded)
import Formatting.Formatters (int, text)
import Lib hiding (domain)
import qualified Lib as URI
import Network.HTTP.Client (HttpException)
import Path
    ( Abs
    , Dir
    , File
    , Path
    , Rel
    , SomeBase (..)
    , addExtension
    , parseAbsFile
    , parseRelDir
    , parseRelFile
    , reldir
    , toFilePath
    , (</>)
    )
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Map as Map (fromList, toList)
import RIO.Partial (succ)
import RIO.Process (HasProcessContext (..))
import qualified RIO.Text as T (break, pack, takeWhile, unpack)
import qualified RIO.Text.Lazy as TL
    ( Text
    , fromStrict
    , take
    , toStrict
    , unpack
    )
import System.Console.ANSI (Color (..))
import System.Directory (createDirectory, makeAbsolute)
import System.Random (randomRIO)
import System.Time.Extra (Seconds, sleep)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (ParseErrorBundle)
import Text.Pretty.Simple (pPrint)
import Text.RawString.QQ (r)
import Text.URI
    ( URI
    , emptyURI
    , mkURI
    , relativeTo
    , render
    , renderBs
    , renderStr
    , unRText
    )
import Text.URI.Lens
    ( authHost
    , uriAuthority
    , uriFragment
    , uriPath
    , uriQuery
    , uriScheme
    )
import Web.Api.WebDriver
    ( ContextId
    , SessionId
    , WebDriverT
    , execWebDriverT
    , executeAsyncScript
    , executeScript
    , getWindowHandle
    , getWindowHandles
    , navigateToStealth
    )


mkWebTable :: forall s env. (HasStateRef s env, HasApp s) => RIO env ()
mkWebTable = do
    sqlBackend <- currentSqlBackend <%= id
    webTab <- queryWebTable sqlBackend
    webTable .= webTab
    domainTable .= Map.fromList (convertEntry <$> Map.toList webTab)
  where
    convertEntry (h, wInfo) = (wInfo ^. webDomain, h)


mkComicTable :: forall s env. (HasStateRef s env, HasApp s) => RIO env ()
mkComicTable = do
    sqlBackend <- currentSqlBackend <%= id
    comicTab <- queryComicTable sqlBackend
    comicTable .= comicTab


mkUrlTable :: forall s env. (HasStateRef s env, HasApp s) => RIO env ()
mkUrlTable = do
    sqlBackend <- currentSqlBackend <%= id
    urlTab <- queryUrlTable sqlBackend
    urlTable .= urlTab


setWebTo :: Web -> forall env s. (HasStateRef s env, HasApp s) => RIO env ()
setWebTo web = do
    currentWeb .= web
    currentPage .= Page 1
    webTab <- webTable <%= id
    webinfo <- maybe (throwM $ LookupWebFailed web) return $ webTab ^? ix web
    currentWebInfo .= webinfo


setComicTo ::
    Comic ->
    forall env s.
    (HasStateRef s env, HasApp s) =>
    RIO env ()
setComicTo comic = do
    currentComic .= comic
    comicTab <- comicTable <%= id
    comicInfo <-
        maybe (throwM $ LookupComicFailed comic) return $
            comicTab ^? ix comic
    currentComicInfo .= comicInfo
    urlTab <- urlTable <%= id
    let revUrlTab = Map.fromList $ swap <$> Map.toList urlTab
    web <- currentWeb <%= id
    url <-
        maybe (throwM $ LookupWebComicFailed (web, comic)) return $
            revUrlTab ^? ix (web, comic)
    currentComicUrl .= url


runWd ::
    forall env s a.
    (HasStateRef s env, HasApp s) =>
    WebDriverT IO a ->
    RIO env a
runWd action = do
    wdSess <- currentWdSession <%= id
    liftIO $ runWdSession wdSess action


runWdSession :: (MonadThrow m, MonadUnliftIO m) => SessionId -> WebDriverT m a -> m a
runWdSession wdSess action = do
    (result, _updatedState, _writerLog) <- runAction action
    case result of
        Right a -> return a
        Left e -> throwM e
  where
    runAction = execWebDriverT $ chromeConfig & initialSession ?~ wdSess
    initialSession = initialState . userState . sessionId


updateNewReleases ::
    forall env s.
    (HasStateRef s env, HasApp s, HasLogFunc s, HasProcessContext s) =>
    RIO env [Try URI]
updateNewReleases = do
    Page page <- currentPage <%= id
    maxPage <- options . maxNumPages <%= id
    if page > maxPage
        then return []
        else do
            domain <- currentWebInfo . webDomain <%= id
            runSimpleApp . logInfo . display $
                vivid Blue
                    <> unRText domain
                    <> (vivid Black <> " [" <> T.pack (show page) <> "]")
            genUrlScript <- currentWebInfo . genUrl <%= id
            pageNUrlVal <- runWd $ executeScript genUrlScript [toJSON page]
            case pageNUrlVal ^.. _String >>= mkURI of
                [] -> throwM ComicLinksNotFound
                pageNUrl : _ -> do
                    runWd $ navigateToStealth $ render $ https domain pageNUrl
                    waitForLoaded
                    waitTime <- getRandomWaitTime
                    liftIO $ sleep waitTime
                    scrapeComicsScript <- currentWebInfo . scrapeComics <%= id
                    newReleases <-
                        runWd (executeScript scrapeComicsScript [])
                            <&> ( view _Array
                                    >>> toList
                                    >>> fmap
                                        ( liftA2 (,) . js2URI
                                            <*> js2MaybeRelInfo
                                        )
                                )
                    when (null newReleases) $ throwM ComicLinksNotFound
                    webSentinel <- currentWebInfo . sentinel <%= id
                    let hit =
                            elemOf
                                (folded . _Right . _1)
                                webSentinel
                                newReleases
                    if hit
                        then
                            traverse tryClickAndDownloadComic $
                                takeWhile
                                    (urlNotMatch webSentinel)
                                    newReleases
                        else do
                            result <-
                                traverse
                                    tryClickAndDownloadComic
                                    newReleases
                            currentPage %= succ
                            (result <>) <$> updateNewReleases
  where
    urlNotMatch = (preview (_Right . _1) >>>) . (/=) . Just


tryParseChapter ::
    (Text -> Try Chapter) -> ToLike (Maybe Text) (Try Chapter)
tryParseChapter mkChapterNo =
    to $ maybeToTry ChapterNoNotFound >>> (>>= mkChapterNo)


tryParseRelInfo ::
    (Text -> Try ReleaseInfo) -> ToLike (Maybe Text) (Try ReleaseInfo)
tryParseRelInfo mkRelInfo =
    to $ maybeToTry ChapterNoNotFound >>> (>>= mkRelInfo)


tryParseURI :: ToLike (Maybe Text) (Try URI)
tryParseURI = to $ maybeToTry ChapterLinksNotFound >>> (>>= mkURI)


js2URI :: Value -> Try URI
js2URI = preview (key "url" . _String) >>> view tryParseURI


js2MaybeURI :: Value -> Try URI
js2MaybeURI x = x ^? _String ^. tryParseURI


js2MaybeRelInfo :: Value -> Try (Maybe ReleaseInfo)
js2MaybeRelInfo = sequence . liftA2 (<>) tryBook tryEpisode
  where
    tryBook =
        preview $
            key "volume"
                . _String
                . to (parseEither decimal)
                . to (fmap $ Book . Volume)
    tryEpisode =
        preview $
            key "chapter"
                . _String
                . to Just
                . tryParseChapter mkChapter
                . to (fmap Episode)


js2RelInfo :: Value -> Try ReleaseInfo
js2RelInfo =
    fromMaybe (Left $ toException ChapterLinksNotFound)
        . liftA2 (<>) tryBook tryEpisode
  where
    tryBook =
        preview $
            key "volume"
                . _String
                . to (parseEither decimal)
                . to (fmap $ Book . Volume)
    tryEpisode =
        preview $
            key "chapter"
                . _String
                . to Just
                . tryParseChapter mkChapter
                . to (fmap Episode)


mapRelativeTo :: forall s. URI -> Lens' s URI -> [Try s] -> [Try s]
mapRelativeTo base focus = map $ applyRelativeTo base focus


-- If the input value is `Left ...`, return the input value untouched.
-- If the input value is `Right (..., relativeURI, ...)`, and
--     1) if `relativeTo` succeeds, return `Right (..., absoluteURI, ...)`
--     2) if `relativeTo` fails, return `Left InvalidBaseUrl`
applyRelativeTo :: forall s. URI -> Lens' s URI -> Try s -> Try s
applyRelativeTo _ _ value@(Left _) = value
applyRelativeTo base focus (Right value) =
    case (value ^. focus) `relativeTo` base of
        Just url -> Right $ value & focus .~ url
        Nothing -> Left $ toException $ InvalidBaseUrl $ renderStr base


getMarkup :: WebDriverT IO TL.Text
getMarkup = do
    markup <- executeScript "return document.getElementsByTagName('html')[0].innerHTML;" []
    let markup' = TL.fromStrict $ markup ^. _String
    if TL.take 6 markup' == "<html>"
        then return markup'
        else return $ "<html>" <> markup' <> "</html>"


tryClickAndDownloadComic ::
    forall env s.
    (HasStateRef s env, HasApp s, HasLogFunc s, HasProcessContext s) =>
    Try (URI, Maybe ReleaseInfo) ->
    RIO env (Try URI)
tryClickAndDownloadComic args = do
    result <- tryAny $ clickAndDownloadComic args
    when (isLeft result) $
        runSimpleApp $
            logError $
                display $
                    vivid Red <> T.pack (displayException $ result ^?! _Left)
    return result


clickAndDownloadComic ::
    forall env s.
    (HasStateRef s env, HasApp s, HasLogFunc s, HasProcessContext s) =>
    Try (URI, Maybe ReleaseInfo) ->
    RIO env URI
clickAndDownloadComic (Left someException) = liftIO $ do
    runSimpleApp $
        logError $
            display $
                vivid Red <> T.pack (displayException someException)
    throwM SomeChaptersNotDownloaded
clickAndDownloadComic (Right (url, maybeRelInfo)) = do
    domain <- currentWebInfo . webDomain <%= id
    if url ^? uriAuthority . _Right . authHost /= Just domain
        then return url
        else do
            urlTab <- urlTable <%= id
            case urlTab ^? ix url of
                Nothing -> return url
                Just (_, comic) -> do
                    setComicTo comic
                    ( Title {unTitle = title}
                        , _
                        , volumeLast
                        , chapterLast
                        ) <-
                        currentComicInfo <%= id
                    if maybeRelInfo `notNewerThan` (volumeLast, chapterLast)
                        then liftIO $ do
                            runSimpleApp $
                                logInfo $
                                    display $
                                        vivid Green
                                            <> title
                                            <> " is up to date"
                            return url
                        else do
                            newReleaseInfo .= maybeRelInfo
                            currentComic .= comic
                            currentComicUrl .= url
                            result <- clickAndScanComic
                            case result of
                                Nothing -> liftIO $ do
                                    runSimpleApp $
                                        logInfo $
                                            display $
                                                vivid Green
                                                    <> title
                                                    <> " is up to date"
                                    return url
                                Just relInfo -> do
                                    updateComicTable comic relInfo
                                    return url


notNewerThan :: Maybe ReleaseInfo -> (Volume, Chapter) -> Bool
notNewerThan relInfo (volumeLast, chapterLast) = case relInfo of
    Nothing -> False
    Just (Episode chapterNew) -> chapterNew <= chapterLast
    Just (Book volumeNew) -> volumeNew <= volumeLast
    Just (Episodes (_, chapterEnd)) -> chapterEnd <= chapterLast


updateComicTable ::
    forall env s.
    (HasStateRef s env, HasApp s) =>
    Comic ->
    ReleaseInfo ->
    RIO env ()
updateComicTable comic = \case
    Episode chapter ->
        updateComicTableChapter comic chapter
    Book volume ->
        updateComicTableVolume comic volume
    Episodes (_, chapter) ->
        updateComicTableChapter comic chapter


updateComicTableChapter ::
    forall env s.
    (HasStateRef s env, HasApp s) =>
    Comic ->
    Chapter ->
    RIO env ()
updateComicTableChapter comic chapter = do
    bracket (currentSqlBackend <%= id) (runSqlConn transactionUndo) $ do
        runSqlConn $ do
            update $ \row -> do
                set_ row [ComicsChapter =. val chapter]
                where_ $ row.comic ==. val comic
            transactionSave
    comicTable . ix comic . _4 .= chapter


updateComicTableVolume ::
    forall env s.
    (HasStateRef s env, HasApp s) =>
    Comic ->
    Volume ->
    RIO env ()
updateComicTableVolume comic volume = do
    sqlBackend <- currentSqlBackend <%= id
    runSql sqlBackend $
        update $ \row -> do
            set_ row [ComicsVolume =. val volume]
            where_ $ row.comic ==. val comic
    comicTable . ix comic . _3 .= volume


openAndScanComic ::
    forall env s.
    (HasStateRef s env, HasApp s, HasLogFunc s, HasProcessContext s) =>
    RIO env (Maybe ReleaseInfo)
openAndScanComic = do
    (Title {unTitle = title}, _, volumeLast, chapterLast) <-
        currentComicInfo <%= id
    runSimpleApp . logInfo . display $
        vivid Black
            <> "scanning "
            <> (vivid Yellow <> title)
            <> ( vivid Black
                    <> " (Vol."
                    <> T.pack (show volumeLast)
                    <> (", Ch." <> T.pack (show chapterLast) <> ")")
               )
    comicUrl <- currentComicUrl <%= id
    runWd $ navigateToStealth $ render comicUrl
    waitForLoaded
    maybeRelInfo <- newReleaseInfo <%= id
    scrapeLatestScript <- currentWebInfo . scrapeLatest <%= id
    latestChapUrl <-
        runWd (executeScript scrapeLatestScript [])
            <&> preview (_String . to Just . tryParseURI)
    let newRelease =
            zipWith
                (liftA2 (,))
                (maybeRelInfo ^.. _Just . to Right)
                (latestChapUrl ^.. _Just)
    scrapeChaptersScript <- currentWebInfo . scrapeChapters <%= id
    newReleases <-
        runWd (executeScript scrapeChaptersScript [])
            <&> ( view _Array
                    >>> toList
                    >>> fmap (liftA2 (,) . js2RelInfo <*> js2URI)
                )
            <&> (<> newRelease)
            <&> uniqRelease
            <&> filterBy (_Right . _1) (`newerThan` (volumeLast, chapterLast))
    let chapters =
            newReleases
                & iover
                    (traversed . _Right)
                    (\n (c, u) -> (c, u, u, length newReleases - n))
                <&> applyRelativeTo comicUrl _2
    result <- traverse tryDownloadRelease chapters
    when (any (has _Left) result) $
        throwM SomeChaptersNotDownloaded
    return $ lastOf (takingWhile isRight folded . _Right) result


filterBy :: Getting (First a) s a -> (a -> Bool) -> [s] -> [s]
filterBy focus pred = toListOf $ folded . filteredBy (focus . filtered pred)


newerThan :: ReleaseInfo -> (Volume, Chapter) -> Bool
newerThan relInfo (volumeLast, chapterLast) = case relInfo of
    Episode chapterNew -> chapterNew > chapterLast
    Book volumeNew -> volumeNew > volumeLast
    Episodes (_, chapterEnd) -> chapterEnd > chapterLast


openAndDownloadRelease ::
    forall env s.
    (HasStateRef s env, HasApp s, HasLogFunc s, HasProcessContext s) =>
    ReleaseInfo ->
    RIO env ()
openAndDownloadRelease relInfo = do
    (Title {unTitle = title}, _, _, _) <- currentComicInfo <%= id
    runSimpleApp . logInfo . display $
        vivid Black
            <> "scanning "
            <> (vivid Yellow <> title)
    comicUrl <- currentComicUrl <%= id
    runWd $ navigateToStealth $ render comicUrl
    waitForLoaded
    scrapeChaptersScript <- currentWebInfo . scrapeChapters <%= id
    targetChapter <-
        runWd (executeScript scrapeChaptersScript [])
            <&> ( view _Array
                    >>> toList
                    >>> fmap (liftA2 (,) . js2RelInfo <*> js2URI)
                )
            <&> uniqRelease
            <&> equalsTo relInfo
    let chapters =
            targetChapter
                & iover
                    (traversed . _Right)
                    (\n (c, u) -> (c, u, u, length targetChapter - n))
                <&> applyRelativeTo comicUrl _2
    result <- traverse tryDownloadRelease chapters
    when (any (has _Left) result) $
        throwM SomeChaptersNotDownloaded
    if lastOf (takingWhile isRight folded . _Right) result == Just relInfo
        then return ()
        else throwM SomeChaptersNotDownloaded
  where
    equalsTo chap =
        toListOf
            (folded . filtered (preview (_Right . _1) >>> (== Just chap)))


openAndDownloadAddress ::
    forall env s.
    (HasStateRef s env, HasApp s, HasLogFunc s, HasProcessContext s) =>
    URI ->
    RIO env ()
openAndDownloadAddress chapUrl = do
    runWd $ navigateToStealth $ render chapUrl
    waitForLoaded
    scrapeImagesScript <- currentWebInfo . scrapeImages <%= id
    images <-
        runWd (executeScript scrapeImagesScript [])
            <&> (view _Array >>> toList >>> fmap (view _String >>> mkURI))
    runningFileName <-
        traverse
            (fmap parseAbsFile . (liftIO . makeAbsolute) . (TL.unpack . format (lpadded 3 '0' int)))
            [1 .. length images]
            <&> toListOf (folded . _Right)
    bracketDownloadRelease $
        bracketDownloadImages $
            traverse_ tryDownloadImage $
                zip images runningFileName
  where
    bracketDownloadRelease = bracket_ printDownloadingChapter printNewLine
    printDownloadingChapter = logSticky =<< stickyLine <.= ""
    printNewLine = logStickyDone =<< stickyLine <%= id
    bracketDownloadImages =
        bracket_
            (logSticky =<< stickyLine <<>= vivid Black <> " [")
            (logSticky =<< stickyLine <<>= vivid Black <> "]")


-- Returns `Just` the last chapter successfully downloaded.
-- Returns `Nothing` if the comic is up to date.
-- May throw `SomeChaptersNotDownloaded`
clickAndScanComic ::
    forall env s.
    (HasStateRef s env, HasApp s, HasLogFunc s, HasProcessContext s) =>
    RIO env (Maybe ReleaseInfo)
clickAndScanComic = do
    (Title {unTitle = title}, _, volumeLast, chapterLast) <-
        currentComicInfo <%= id
    runSimpleApp . logInfo . display $
        vivid Black
            <> "scanning "
            <> (vivid Yellow <> title)
            <> ( vivid Black
                    <> " (Vol."
                    <> T.pack (show volumeLast)
                    <> (", Ch." <> T.pack (show chapterLast) <> ")")
               )
    comicUrl <- currentComicUrl <%= id
    runWd $ navigateToStealth $ render comicUrl
    waitForLoaded
    maybeRelInfo <- newReleaseInfo <%= id
    scrapeLatestScript <- currentWebInfo . scrapeLatest <%= id
    latestChapUrl <-
        fromRight (throwM LatestChapterNotFound)
            <$> tryAny
                ( runWd (executeScript scrapeLatestScript [])
                    <&> preview (_String . to Just . tryParseURI)
                )
    let newRelease =
            zipWith
                (liftA2 (,))
                (maybeRelInfo ^.. _Just . to Right)
                (latestChapUrl ^.. _Just)
    scrapeChaptersScript <- currentWebInfo . scrapeChapters <%= id
    releases <-
        runWd (executeScript scrapeChaptersScript [])
            <&> ( view _Array
                    >>> toList
                    >>> fmap (liftA2 (,) . js2RelInfo <*> js2URI)
                )
            <&> (<> newRelease)
            <&> uniqRelease
    when (null releases) $ throwM ChapterLinksNotFound
    let newReleases =
            releases
                & filterBy
                    (_Right . _1)
                    (`newerThan` (volumeLast, chapterLast))
        chapters =
            newReleases
                & iover
                    (traversed . _Right)
                    (\n (c, u) -> (c, u, u, length newReleases - n))
                <&> applyRelativeTo comicUrl _2
    result <- traverse tryDownloadRelease chapters
    when (any (has _Left) result) $
        throwM SomeChaptersNotDownloaded
    return $ lastOf (takingWhile isRight folded . _Right) result


uniqRelease :: [Try (ReleaseInfo, URI)] -> [Try (ReleaseInfo, URI)]
uniqRelease [] = []
uniqRelease [x] = [x]
uniqRelease (x1 : x2 : xs) =
    if x1 ^? _Right . _1 == x2 ^? _Right . _1
        then uniqRelease (x2 : xs)
        else x1 : uniqRelease (x2 : xs)


tryDownloadRelease ::
    forall env s.
    (HasStateRef s env, HasApp s, HasLogFunc s, HasProcessContext s) =>
    Try (ReleaseInfo, URI, URI, Int) ->
    RIO env (Try ReleaseInfo)
tryDownloadRelease args = case args of
    Left someException -> do
        printException someException
        return $ Left someException
    Right args' -> do
        result <- tryAny $ downloadRelease args'
        case result of
            Left someException -> do
                printException someException
                return result
            _ -> return result


printException :: Exception e => e -> RIO env ()
printException someException = liftIO $ do
    runSimpleApp $
        logError $
            display $
                vivid Red <> T.pack (displayException someException)


downloadRelease ::
    forall env s.
    (HasStateRef s env, HasApp s, HasLogFunc s, HasProcessContext s) =>
    (ReleaseInfo, URI, URI, Int) ->
    RIO env ReleaseInfo
downloadRelease (relInfo, absUrl, _relUrl, _count) =
    bracketDownloadRelease $ do
        rootFolder <- options . rootDir <%= id
        comicFolder <- currentComicInfo . _2 <%= id
        let releaseDir = releaseFolder relInfo
            targetFolder = rootFolder </> comicFolder </> releaseDir
        runWd $ navigateToStealth $ render absUrl
        waitForLoaded
        images <- waitForImagesToLoad
        when (null images) $
            throwM SomeImagesNotDownloaded
        liftIO $ createDirectory $ toFilePath targetFolder
        currentReferer .= absUrl
        runningFileName <-
            traverse
                (parseRelFile . (TL.unpack . format (lpadded 3 '0' int)))
                [1 .. length images]
        result <-
            bracketDownloadImages $
                traverse tryDownloadImage $
                    zip images ((targetFolder </>) <$> runningFileName)
        traverseOf_ (each . _Left) printExceptionEx result
        when (any (has _Left) result) $
            throwM SomeImagesNotDownloaded
        return relInfo
  where
    bracketDownloadRelease = bracket_ printDownloadingChapter printNewLine

    printDownloadingChapter = do
        logSticky
            =<< stickyLine
                <.= ( vivid Black <> "downloading " <> case relInfo of
                        Book _ -> "volume "
                        _ -> "chapter "
                    )
                    <> (vivid Yellow <> T.pack (showReleaseInfo relInfo))

    printNewLine = do
        logStickyDone =<< stickyLine <%= id

    bracketDownloadImages =
        bracket_
            (logSticky =<< stickyLine <<>= vivid Black <> " [")
            (logSticky =<< stickyLine <<>= vivid Black <> "]")

    printExceptionEx someException = case fromException someException of
        Just e -> do
            runSimpleApp $
                logWarn $
                    display $
                        vivid Yellow
                            <> T.pack
                                ( displayException
                                    (e :: InvalidImageSubtype)
                                )
        _ -> printException someException

    waitForImagesToLoad = do
        scrapeImagesScript <- currentWebInfo . scrapeImages <%= id
        images <-
            runWd (executeScript scrapeImagesScript [])
                <&> ( view _Array
                        >>> toList
                        >>> fmap (preview _String >>> view tryParseURI)
                    )
        if has
            ( folded
                . _Right
                . to (not . null . T.indices "image/gif" . render)
                . filtered id
            )
            images
            then do
                liftIO $ sleep 1
                waitForImagesToLoad
            else return images


releaseFolder :: ReleaseInfo -> Path Rel Dir
releaseFolder relInfo =
    fromMaybe [reldir|unknown|] $
        parseRelDir $
            TL.unpack $ case relInfo of
                Episode chapter -> chapterFolder chapter
                Book volume -> volumeFolder volume
                Episodes chapterRange ->
                    chapterRangeFolder chapterRange


chapterFolder :: Chapter -> TL.Text
chapterFolder (Chapter chapter section) = case section of
    Nothing ->
        format (lpadded 3 '0' int) chapter
    Just section' ->
        format (lpadded 3 '0' int % "." % int) chapter section'


volumeFolder :: Volume -> TL.Text
volumeFolder (Volume volume) = format (lpadded 2 '0' int) volume


chapterRangeFolder :: (Chapter, Chapter) -> TL.Text
chapterRangeFolder (chapterBegin, chapterEnd) =
    format
        (text % "-" % text)
        (chapterFolder chapterBegin)
        (chapterFolder chapterEnd)


showReleaseInfo :: ReleaseInfo -> String
showReleaseInfo = \case
    Episode chapter -> show chapter
    Book volume -> show $ unVolume volume
    Episodes (chapterBegin, chapterEnd) ->
        show chapterBegin <> "-" <> show chapterEnd


tryDownloadImage ::
    forall env s.
    (HasStateRef s env, HasApp s, HasLogFunc s, HasProcessContext s) =>
    (Try URI, Path Abs File) ->
    RIO env (Try ())
tryDownloadImage (tryUrl, imgFileName) = case tryUrl of
    Left someException -> return $ Left someException
    Right url -> do
        result <- tryAny $ do
            downloadImage (url, imgFileName)
            putTick Green
        case result of
            Right () -> return result
            Left someException -> case fromException someException of
                Just e -> returnLeft Red $ toException (e :: IOException)
                _ -> case fromException someException of
                    Just e ->
                        returnLeft Red $
                            toException (e :: HttpException)
                    _ -> case fromException someException of
                        Just e ->
                            returnLeft Red $
                                toException (e :: InvalidContentType)
                        _ -> case fromException someException of
                            Just e ->
                                returnLeft Yellow $
                                    toException (e :: InvalidImageSubtype)
                            _ -> throwM someException
  where
    putTick color = do
        logSticky =<< stickyLine <<>= vivid color <> T.pack tick

    tick = case reverse $ toFilePath imgFileName of
        oneth : tenth : _ ->
            case oneth of
                '0' -> tenth : "0"
                '5' -> "|"
                _ -> "·"
        _ -> "!"

    returnLeft color someException =
        putTick color >> return (Left someException)


getRandomWaitTime ::
    forall env s.
    (HasStateRef s env, HasApp s) =>
    RIO env Seconds
getRandomWaitTime = do
    web <- currentWeb <%= id
    if web `elem` [mangaRawSo, mangaRawIo, manga1001Su]
        then randomRIO (minWaitTime, maxWaitTime)
        else return defaultWaitTime


-- Wait some seconds, for a new `WindowHandle` not exist in `[WindowHandle]`
-- and return the new `WindowHandle`
waitForNewWin ::
    forall env s.
    (HasStateRef s env, HasApp s) =>
    [ContextId] ->
    RIO env ContextId
waitForNewWin allExistingWins = do
    newWins <- runWd getWindowHandles
    waitTime <- getRandomWaitTime
    case newWins `exclude` allExistingWins of
        [] -> do
            liftIO $ sleep waitTime
            waitForNewWin allExistingWins
        newlyWindow : _ -> return newlyWindow


-- Wait repeatedly until the closing `WindowHandle` not exist in `[WindowHandle]`
waitForClosingWin ::
    forall env s.
    (HasStateRef s env, HasApp s) =>
    ContextId ->
    RIO env ()
waitForClosingWin closingWin = do
    allWins <- runWd getWindowHandles
    waitTime <- getRandomWaitTime
    when (closingWin `elem` allWins) $ do
        liftIO $ sleep waitTime
        waitForClosingWin closingWin


waitForLoaded :: forall env s. (HasStateRef s env, HasApp s) => RIO env ()
waitForLoaded = do
    isLoadedScript <- currentWebInfo . isLoaded <%= id
    runWd $ waitUntil isLoadedScript $ round $ maxWaitTime * 1000


waitUntil :: MonadUnliftIO m => Text -> Int -> WebDriverT m ()
waitUntil func msec =
    void $ executeScript jsWaitUntil [toJSON func, toJSON msec]
  where
    jsWaitUntil =
        [r|
return await new Promise((resolve, reject) => {
  const timeWas = new Date();
  const pred = new Function("return (" + arguments[0] + ");");
  const timeout = arguments[1];
  const wait = setInterval(function() {
    const timespan = new Date() - timeWas;
    if (pred() || timespan > timeout) {
      clearInterval(wait);
      resolve();
    }
  }, 200);
})
        |]


execNewWin :: forall env s. (HasStateRef s env, HasApp s) => Text -> [Value] -> RIO env ContextId
execNewWin codeJS args = do
    existingTabs <- runWd getWindowHandles
    void $ runWd $ executeScript codeJS args
    waitForNewWin existingTabs


downloadImage :: forall env s. (HasStateRef s env, HasApp s) => (URI, Path Abs File) -> RIO env ()
downloadImage (url, filename) = do
    base64 <- runWd $ executeAsyncScript xmlHttpRequest [toJSON $ render url]
    case parseOnly parseEmbeddedData $ encodeUtf8 $ base64 ^. _String of
        Right (ext, binaryData) ->
            case addExtension ("." <> T.unpack ext) filename of
                Right fullFileName -> liftIO $ BS.writeFile (toFilePath fullFileName) binaryData
                Left pathError ->
                    throwM $ InvalidContentType $ displayException pathError
        Left parseError ->
            throwM $ InvalidContentType parseError


downloadImageAux :: SessionId -> (URI, Path Abs File) -> IO ()
downloadImageAux sess (url, filename) = do
    base64 <- runWdSession sess $ executeAsyncScript xmlHttpRequest [toJSON $ render url]
    case parseOnly parseEmbeddedData $ encodeUtf8 $ base64 ^. _String of
        Right (ext, binaryData) ->
            case addExtension ("." <> T.unpack ext) filename of
                Right fullFileName -> do
                    liftIO $ BS.writeFile (toFilePath fullFileName) binaryData
                    pPrint filename
                Left pathError ->
                    throwM $ InvalidContentType $ displayException pathError
        Left parseError ->
            throwM $ InvalidContentType parseError


xmlHttpRequest :: Text
xmlHttpRequest =
    [r|
        const url = arguments[0];
        const resolve = arguments[arguments.length - 1];
        const magic = "xmlHttpRequest";
        const input = document.createElement("input");
        const uid = () => {
            const tmp = performance.now().toString(36).replace(/\./g, "");
            return document.getElementById(tmp) == undefined ? tmp : uid();
        };
        input.id = uid();
        input.type = "hidden";
        document.querySelectorAll("body")[0].append(input);
        const handler = () => {
            input.removeEventListener(input.id, handler);
            const result = input.value;
            delete input.id;
            input.remove();
            resolve(result);
        };
        input.addEventListener(input.id, handler);
        const location = window.location;
        const origin = location.protocol + "//" + location.hostname + "/";
        window.postMessage(JSON.stringify({
            magic: magic,
            url: url,
            id: input.id
        }), origin);
    |]


updateWebTable :: forall env s. (HasStateRef s env, HasApp s) => Web -> URI -> RIO env ()
updateWebTable web absUrl = do
    sqlBackend <- currentSqlBackend <%= id
    runSql sqlBackend $
        update $ \row -> do
            set_ row [WebsSentinel =. val absUrl]
            where_ $ row.web ==. val web


selectorAnchor :: URI -> Text
selectorAnchor url =
    "a[href='"
        <> render url
        <> "']"
        <> (",a[href='" <> render (url & removeHttps & removeDomain True) <> "']")
        <> (",a[href='" <> render (url & removeHttps & removeDomain False) <> "']")
        <> (",a[href='" <> render url <> "/']")
        <> (",a[href='" <> render (url & removeHttps & removeDomain True) <> "/']")
        <> (",a[href='" <> render (url & removeHttps & removeDomain False) <> "/']")
  where
    removeHttps = uriScheme .~ Nothing
    removeDomain isPathAbsolute = uriAuthority .~ Left isPathAbsolute


clickAnchorTargetBlank :: URI -> Text
clickAnchorTargetBlank url =
    [r|
        do {
            var tmp = document.querySelectorAll("|]
        <> selectorAnchor url
        <> [r|")[0];
            tmp.target = '_blank';
            tmp.click();
        } while (false);
    |]


urlToWeb :: forall env s. (HasStateRef s env, HasApp s) => URI -> RIO env (Maybe Web)
urlToWeb url = do
    domainTab <- domainTable <%= id
    return $ url ^? URI.domain >>= \domain' -> domainTab ^? ix domain'


{-- Wait until the element whose id is `elemId :: Text` is visible.
waitUntilVisible :: Text -> WD ()
waitUntilVisible elemId = do
    waitUntilJS $
        "window.getComputedStyle"
            <> ("(document.getElementById('" <> elemId <> "'))")
            <> ".display !== 'none'"
--}

-- Wait until `condition :: Text` is `True`.
waitUntilJS :: forall env s. (HasStateRef s env, HasApp s) => Text -> RIO env ()
waitUntilJS condition = do
    waitTime <- getRandomWaitTime
    satisfied <- runWd $ executeScript ("return ((" <> condition <> ") ? true : false);") []
    liftIO $ sleep waitTime
    unless (satisfied == Bool True) $ waitUntilJS condition


scanWeb ::
    forall env s.
    (HasStateRef s env, HasApp s, HasLogFunc s, HasProcessContext s) =>
    Web ->
    RIO env ()
scanWeb web = do
    setWebTo web
    result <- updateNewReleases
    let lastSuccess = result ^.. takingWhile (has _Right) (backwards folded) . _Right & lastOf folded
    traverse_ (updateWebTable web) lastSuccess

{--
testComics
    :: forall env s
     . (HasStateRef s env, HasApp s)
    => Web
    -> RIO env ()
testComics web = do
    setWebTo web
    getNewReleaseUrl >>= \url -> runWd (navigateToStealth $ render url) *> waitForLoaded
    runWd getMarkup >>= scrapeComics >>= pPrint . fmap (fmap $ first URL)

    void $ currentPage <%= succ
    getNewReleaseUrl >>= \url -> runWd (navigateToStealth $ render url) *> waitForLoaded
    runWd getMarkup >>= scrapeComics >>= pPrint . fmap (fmap $ first URL)

testChapter
    :: forall env s
     . (HasStateRef s env, HasApp s)
    => Web
    -> RIO env ()
testChapter web = do
    setWebTo web
    getNewReleaseUrl >>= \url -> runWd (navigateToStealth $ render url) *> waitForLoaded
    runWd getMarkup >>= scrapeComics >>= (take 1 >>> traverse_ doTest)
  where
    doTest (Left _) = return ()
    doTest (Right (url, _)) = do
        runWd $ navigateToStealth $ render url
        waitForLoaded
        runWd getMarkup >>= scrapeRelInfos >>= pPrint . fmap (fmap $ fmap URL)
--}
