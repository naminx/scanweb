{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

#if MIN_VERSION_GLASGOW_HASKELL(9,2,0,0)
{-# LANGUAGE OverloadedRecordDot #-}
#endif

module App.Tables where

import Database.Esqueleto.Experimental (
    InnerJoin (InnerJoin),
    SqlBackend,
    Value (unValue),
    from,
    on,
    select,
    table,
    (:&) ((:&)),
    (==.),
 )
import Import hiding (domain, from, on, (^.))
import qualified RIO.List as L (foldl)
import qualified RIO.Map as Map (fromList)
import qualified RIO.Text as T (pack)
import System.Console.ANSI.Types (Color (Red))
import Text.URI (UserInfo (UserInfo))


queryWebTable :: (MonadUnliftIO m, MonadThrow m) => SqlBackend -> m WebTable
queryWebTable sqlBackend = do
    rows <- runSql sqlBackend $
        select $ do
            webs <- from $ table @Webs
#if MIN_VERSION_GLASGOW_HASKELL(9,2,0,0)
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
    webs <- L.foldl tryParseRow (return []) rows
    return $ Map.fromList webs
  where
    tryParseRow ::
        (MonadUnliftIO m, MonadThrow m) =>
        m [(Web, WebInfo)] ->
        ( Value Web
        , Value Domain
        , Value (Maybe (RText 'Username))
        , Value (Maybe (RText 'Password))
        , Value URI
        , Value Text
        , Value Text
        , Value Text
        , Value Text
        , Value Text
        , Value Text
        ) ->
        m [(Web, WebInfo)]
    tryParseRow acc args = do
        result <- tryAny $ parseRow args
        case result of
            Right row -> acc <&> (<> [row])
            Left someException -> do
                runSimpleApp . logError . display $
                    vivid Red <> T.pack (displayException someException)
                acc

    parseRow ::
        MonadThrow m =>
        ( Value Web
        , Value Domain
        , Value (Maybe (RText 'Username))
        , Value (Maybe (RText 'Password))
        , Value URI
        , Value Text
        , Value Text
        , Value Text
        , Value Text
        , Value Text
        , Value Text
        ) ->
        m (Web, WebInfo)
    parseRow
        ( web
            , domain
            , user
            , pwd
            , path
            , genUrlVal
            , isLoadedVal
            , scrapeComicsVal
            , scrapeLatestVal
            , scrapeChaptersVal
            , scrapeImagesVal
            ) = do
            return
                ( unValue web
                , emptyWebInfo
                    & webDomain .~ unValue domain
                    & userInfo .~ (UserInfo <$> unValue user ?? unValue pwd)
                    & sentinel .~ unValue path
                    & genUrl .~ unValue genUrlVal
                    & isLoaded .~ unValue isLoadedVal
                    & scrapeComics .~ unValue scrapeComicsVal
                    & scrapeLatest .~ unValue scrapeLatestVal
                    & scrapeChapters .~ unValue scrapeChaptersVal
                    & scrapeImages .~ unValue scrapeImagesVal
                )


queryComicTable :: (MonadUnliftIO m, MonadThrow m) => SqlBackend -> m ComicTable
queryComicTable sqlBackend = do
    rows <- runSql sqlBackend $
#if MIN_VERSION_GLASGOW_HASKELL(9,2,0,0)
        select $ do
            comics :& _urls <-
                from $
                    table @Comics `InnerJoin` table @Urls
                        `on` (\(comics :& urls) -> comics.comic ==. urls.comic)
            pure (comics.comic, comics.title, comics.folder, comics.volume, comics.chapter)
#else
        select $ do
            comics :& _urls <-
                from $
                    table @Comics `InnerJoin` table @Urls
                        `on` (\(comics :& urls) -> comics .^ ComicsComic ==. urls .^ UrlsComic)
            pure ( comics .^ ComicsComic, comics .^ ComicsTitle
                 , comics .^ ComicsFolder
                 , comics .^ ComicsVolume, comics .^ ComicsChapter)
#endif
    comics <- L.foldl tryParseRow (return []) rows
    return $ Map.fromList comics
  where
    tryParseRow ::
        (MonadUnliftIO m, MonadThrow m) =>
        m [(Comic, (Title, Path Rel Dir, Volume, Chapter))] ->
        (Value Comic, Value Title, Value (Path Rel Dir), Value Volume, Value Chapter) ->
        m [(Comic, (Title, Path Rel Dir, Volume, Chapter))]
    tryParseRow acc args = do
        result <- tryAny $ parseRow args
        case result of
            Right row -> acc <&> (<> [row])
            Left someException -> do
                runSimpleApp . logError . display $
                    vivid Red <> T.pack (displayException someException)
                acc

    parseRow ::
        MonadThrow m =>
        (Value Comic, Value Title, Value (Path Rel Dir), Value Volume, Value Chapter) ->
        m (Comic, (Title, Path Rel Dir, Volume, Chapter))
    parseRow (comic, title, folder, volume, chapter) = do
        return
            ( unValue comic
            ,
                ( unValue title
                , unValue folder
                , unValue volume
                , unValue chapter
                )
            )


queryUrlTable :: (MonadUnliftIO m, MonadThrow m) => SqlBackend -> m UrlTable
queryUrlTable sqlBackend = do
    rows <- runSql sqlBackend $
#if MIN_VERSION_GLASGOW_HASKELL(9,2,0,0)
        select $ do
            urls :& webs <-
                from $
                    table @Urls `InnerJoin` table @Webs
                        `on` (\(urls :& webs) -> urls.web ==. webs.web)
            pure (urls.web, urls.comic, webs.domain, urls.path)
#else
        select $ do
            urls :& webs <-
                from $
                    table @Urls `InnerJoin` table @Webs
                        `on` (\(urls :& webs) -> urls .^ UrlsWeb ==. webs .^ WebsWeb)
            pure (urls .^ UrlsWeb, urls .^ UrlsComic, webs .^ WebsDomain, urls .^ UrlsPath)
#endif
    urls <- L.foldl tryParseRow (return []) rows
    return $ Map.fromList urls
  where
    tryParseRow ::
        (MonadUnliftIO m, MonadThrow m) =>
        m [(URI, (Web, Comic))] ->
        (Value Web, Value Comic, Value Domain, Value URI) ->
        m [(URI, (Web, Comic))]
    tryParseRow acc args = do
        result <- tryAny $ parseRow args
        case result of
            Right row -> acc <&> (<> [row])
            Left someException -> do
                runSimpleApp . logError . display $
                    vivid Red <> T.pack (displayException someException)
                acc

    parseRow ::
        MonadThrow m =>
        (Value Web, Value Comic, Value Domain, Value URI) ->
        m (URI, (Web, Comic))
    parseRow (web, comic, domain, path) = do
        return (https (unValue domain) (unValue path), (unValue web, unValue comic))
