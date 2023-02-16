{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

#if MIN_VERSION_GLASGOW_HASKELL(9,2,0,0)
{-# LANGUAGE OverloadedRecordDot #-}
#endif

module Main where

import App.Config
import App.Tables
import App.Types
import Control.Monad.Extra (concatMapM, whileM)
import Control.Monad.Logger
import Control.Monad.Loops (untilM_)
import Control.Monad.Trans.Resource
import Data.Aeson.Lens
import Data.Aeson
import Data.Maybe (fromJust)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import Data.Tuple.Extra (dupe)
import Database.Esqueleto.Experimental hiding ((<&>), (^.))
import qualified Database.Esqueleto.Experimental as ES
import Database.Esqueleto.Internal.Internal (unsafeSqlBinOp)
import Import hiding (from, on)
import Init
import Options
import Path (Abs, Dir, File, Path, Rel, SomeBase (..), parseAbsFile, (</>))
import Progs
import RIO.Process
import RIO.State
import qualified RIO.Text as T
import qualified RIO.Text.Lazy as TL
import Replace.Megaparsec (anyTill, breakCap)
import Run (downloadImageAux, runWdSession, waitUntil)
import System.Console.ANSI (SGR (..), setSGR)
import System.Console.ANSI.Types (Color (..))
import System.Directory (getCurrentDirectory, makeAbsolute)
import System.Process (CreateProcess (..), StdStream (..), withCreateProcess)
import qualified System.Process as P
import System.Time.Extra (sleep)
import Text.Megaparsec (
    Parsec,
    ShowErrorComponent (..),
    eof,
    many,
    parseTest,
    some,
    try,
 )
import Text.Megaparsec.Char (space1, string)
import Text.Pretty.Simple (pPrint)
import Text.Printf
import Text.RawString.QQ (r)
import Text.URI (
    Authority (Authority),
    mkURI,
    render,
    unRText,
    UserInfo (UserInfo)
 )
import Text.URI.Lens (authHost, uriAuthority)
import Text.URI.QQ (host, uri, username)
import Web.Api.WebDriver as WD
import qualified Prelude


main :: IO ()
main = bracket_
    (return ())
    (liftIO $ setSGR [Reset])
    $ do
        cliOptions <- parseOptions
        let dbFilePath = case cliOptions ^. dbFile of
                Abs absFilePath -> absFilePath
                Rel relFilePath -> cliOptions ^. rootDir </> relFilePath
        appLogOptions <- logOptionsHandle stdout False
        procContext <- mkDefaultProcessContext
        withLogFunc appLogOptions $ \logFunction ->
            bracket (newSqlBackend dbFilePath) close' $ \conn ->
                if (cliOptions ^. appMode) `notElem` [ListWebs, ListComics]
                    then withCreateProcess chromeDriverProc $
                        \_ houtResult _ _ -> case houtResult of
                            Just hout -> do
                                void $ waitForChromeDriver hout
                                withAsync (consumeInput hout) $
                                    const $
                                        bracket
                                            newWdSession
                                            (`runWdSession` deleteSession)
                                            $ \wdSess -> do
                                                myApp <-
                                                    newSomeRef $
                                                        initApp
                                                            logFunction
                                                            procContext
                                                            cliOptions
                                                            conn
                                                            wdSess
                                                runRIO myApp $ do
                                                    setupEnv
                                                    getProg $ cliOptions ^. appMode
                            Nothing -> return ()
                    else do
                        myApp <-
                            newSomeRef $
                                initApp
                                    logFunction
                                    procContext
                                    cliOptions
                                    conn
                                    undefined
                        runRIO myApp $ do
                            setupEnv
                            getProg $ cliOptions ^. appMode
  where
    chromeDriverProc = (P.proc chromeDriverExe []) {std_out = CreatePipe}
    waitForChromeDriver = whileM . (T.hGetLine >>> ((/= ready) <$>))
      where
        ready = "ChromeDriver was started successfully."
    consumeInput = forever . T.hGetLine


getProg ::
    forall env s.
    (HasStateRef s env, HasApp s, HasLogFunc s, HasProcessContext s) =>
    AppMode ->
    RIO env ()
getProg appmode = case appmode of
    ScanWebs webs -> progScanWebs webs
    UpdateComic (web, comic, relInfo) -> progUpdateComic (web, comic, relInfo)
    DownloadRelease (web, comic, relInfo) ->
        progDownloadRelease (web, comic, relInfo)
    DownloadAddress url ->
        progDownloadChapter url
    ListWebs -> progListWebs
    ListComics -> progListComics


queryComics :: MonadUnliftIO m => [URI] -> m [(Comic, URI, ComicInfo)]
queryComics [] = return []
queryComics links =
    bracket (newSqlBackend defaultDbFile) (liftIO . close') $
        (map unValues <$>) . runSqlConn query
  where
#if MIN_VERSION_GLASGOW_HASKELL(9,2,0,0)
    query = select $ do
        webs :& urls :& comics <-
            from $
                ( table @Webs `InnerJoin` table @Urls
                    `on` (\(webs :& urls) -> webs.web ==. urls.web)
                )
                    `InnerJoin` table @Comics
                    `on` (\(_ :& urls :& comics) -> urls.comic ==. comics.comic)
        let fullUrl = val [uri|https://|] ++. castString webs.domain ++. urls.path
        where_ $ fullUrl `in_` valList links
        orderBy $ map (asc . (fullUrl !=.) . val) links
        pure
            ( urls.comic
            , fullUrl
            , (comics.title, comics.folder, comics.volume, comics.chapter)
            )
#else
    query = select $ do
        webs :& urls :& comics <-
            from $
                ( table @Webs `InnerJoin` table @Urls
                    `on` (\(webs :& urls) -> webs .^ WebsWeb ==. urls .^ UrlsWeb)
                )
                    `InnerJoin` table @Comics
                    `on` (\(_ :& urls :& comics) -> urls .^ UrlsComic ==. comics .^ ComicsComic)
        let fullUrl = val [uri|https://|] ++. castString (webs .^ WebsDomain) ++. urls .^ UrlsPath
        where_ $ fullUrl `in_` valList links
        orderBy $ map (asc . (fullUrl !=.) . val) links
        pure
            ( urls .^ UrlsComic
            , fullUrl
            , (comics .^ ComicsTitle, comics .^ ComicsFolder, comics .^ ComicsVolume, comics .^ ComicsChapter)
            )
#endif
    unValues (comic, uri_, comicInfo) =
        (unValue comic, unValue uri_, unValues' comicInfo)
      where
        unValues' (title, path, volume, chapter) =
            (unValue title, unValue path, unValue volume, unValue chapter)


queryComic :: MonadUnliftIO m => Int -> m (Maybe ComicInfo)
queryComic c =
    bracket (newSqlBackend defaultDbFile) (liftIO . close') $
        (fmap unValues . preview _head <$>) . runSqlConn query
  where
#if MIN_VERSION_GLASGOW_HASKELL(9,2,0,0)
    query = select $ do
        comics <- from $ table @Comics
        where_ $ comics.comic ==. val (Comic c)
        pure (comics.title, comics.folder, comics.volume, comics.chapter)
#else
    query = select $ do
        comics <- from $ table @Comics
        where_ $ comics .^ ComicsComic ==. val (Comic c)
        pure (comics .^ ComicsTitle, comics .^ ComicsFolder, comics .^ ComicsVolume, comics .^ ComicsChapter)
#endif
    unValues (title, path, volume, chapter) =
        (unValue title, unValue path, unValue volume, unValue chapter)


queryWeb :: (MonadFail f, MonadUnliftIO m) => URI -> m (f WebInfo)
queryWeb url =
    bracket (newSqlBackend defaultDbFile) (liftIO . close') $
       runSqlConn query
            >>> fmap (preview _head >>> toMonadFail)
  where
    toMonadFail = maybe (fail "Unknown web") (return . toWebInfo)
    toWebInfo
        ( _
        , webDomain_
        , webUsername
        , webPassword
        , webSentinel
        , webGenUrl
        , webIsLoaded
        , webScrapeComics
        , webScrapeLatest
        , webScrapeChapters
        , webScrapeImages
        ) =
        WebInfo
            { _webDomain = unValue webDomain_
            , _userInfo = Just $ UserInfo
                  (fromMaybe [username|user|] $ unValue webUsername)
                  (unValue webPassword)
            , _sentinel = unValue webSentinel
            , _genUrl = unValue webGenUrl
            , _isLoaded = unValue webIsLoaded
            , _scrapeComics = unValue webScrapeComics
            , _scrapeLatest = unValue webScrapeLatest
            , _scrapeChapters = unValue webScrapeChapters
            , _scrapeImages = unValue webScrapeImages
            }
#if MIN_VERSION_GLASGOW_HASKELL(9,2,0,0)
    query = select $ do
        webs <- from $ table @Webs
        where_ $ webs.domain ==. val (url ^?! domain)
        pure
            ( webs.web
            , webs.domain
            , webs.username
            , webs.password
            , webs.sentinel
            , webs.genUrl
            , webs.isLoaded
            , webs.scrapeComics
            , webs.scrapeLatest
            , webs.scrapeChapters
            , webs.scrapeImages
            )
#else
    query = select $ do
        webs <- from $ table @Webs
        where_ $ webs .^ WebsDomain ==. val (url ^?! domain)
        pure
            ( webs .^ WebsWeb
            , webs .^ WebsDomain
            , webs .^ WebsUsername
            , webs .^ WebsPassword
            , webs .^ WebsSentinel
            , webs .^ WebsGenUrl
            , webs .^ WebsIsLoaded
            , webs .^ WebsScrapeComics
            , webs .^ WebsScrapeLatest
            , webs .^ WebsScrapeChapters
            , webs .^ WebsScrapeImages
            )
#endif


getComicsFrom ::
    (MonadThrow m, MonadUnliftIO m) =>
    SessionId -> URI -> Int ->
    m [(Comic, URI, ComicInfo)]
getComicsFrom sess domain' page = do
    webinfo <- queryWeb domain'
    concatMapM getComicsAux webinfo
  where
    getComicsAux :: (MonadThrow m, MonadUnliftIO m) =>
        WebInfo -> m [(Comic, URI, ComicInfo)]
    getComicsAux webinfo = do
        paths <- runWdSession sess $
            executeScript (webinfo ^. genUrl) [toJSON page]
                <&> toListOf (_String . toURI)
        traverse getComicAux2 paths
      where
        getComicAux2 :: (MonadThrow m, MonadUnliftIO m) =>
            URI -> m (Comic, URI, ComicInfo)
        getComicAux2 url = runWdSession sess $ do
           navigateToStealth $ render $ https (webinfo ^. webDomain) url
           liftIO (sleep 1000)
               `untilM_` executeScript (web' ^. isLoaded) [] <&> (^?! _Bool)
           executeScript (webinfo ^. scrapeComics) []
                <&> toListOf (_Array . to toList . to (fmap $ preview $ key "url" . _String . toURI) . to catMaybes)
