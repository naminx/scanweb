{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 902
{-# LANGUAGE OverloadedRecordDot #-}
#endif
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module App.Tables where

import App.Chapter (Chapter (Chapter))
import App.Exceptions (InvalidWebNo (InvalidWebNo))
import Data.WOE (toEnumSafely)
import Database.Esqueleto.Experimental (
#if __GLASGOW_HASKELL__ < 902
    From (Table),
#endif
    InnerJoin (InnerJoin),
    SqlBackend,
    Value (unValue),
    from,
    on,
    runSqlConn,
    select,
#if __GLASGOW_HASKELL__ >= 902
    table,
#endif
    (:&) ((:&)),
    (==.),
#if __GLASGOW_HASKELL__ < 902
    (^.),
#endif
 )
import Import hiding (domain, from, on, (^.))
import Path (Dir, Path, Rel, parseRelDir)
import qualified RIO.List as L (foldl)
import qualified RIO.Map as Map (fromList)
import qualified RIO.Text as T (pack, unpack)
import System.Console.ANSI.Types (Color (Red))
import Text.URI (UserInfo (UserInfo), mkHost, mkPassword, mkURI, mkUsername)


queryWebTable :: (MonadUnliftIO m, MonadThrow m) => SqlBackend -> m WebTable
queryWebTable sqlBackend = do
    rows <- flip runSqlConn sqlBackend $
        select $ do
#if __GLASGOW_HASKELL__ >= 902
            webs <- from $ table @Webs
            pure (webs.web, webs.domain, webs.username, webs.password, webs.sentinel)
#else
            webs <- from $ Table @Webs
            pure ( webs ^. WebsWeb, webs ^. WebsDomain, webs ^. WebsUsername
                 , webs ^. WebsPassword, webs ^. WebsSentinel)
#endif
    webs <- L.foldl tryParseRow (return []) rows
    return $ Map.fromList webs
  where
    tryParseRow ::
        (MonadUnliftIO m, MonadThrow m) =>
        m [(Web, WebInfo)] ->
        (Value Int, Value Text, Value (Maybe Text), Value (Maybe Text), Value Text) ->
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
        (Value Int, Value Text, Value (Maybe Text), Value (Maybe Text), Value Text) ->
        m (Web, WebInfo)
    parseRow (web, domain, user, pwd, path) = case toEnumSafely $ unValue web of
        Nothing -> throwM $ InvalidWebNo $ unValue web
        Just web' -> do
            domain' <- mkHost $ unValue domain
            user' <- mapM mkUsername $ unValue user
            pwd' <- mapM mkPassword $ unValue pwd
            path' <- mkURI $ unValue path
            return (web', (domain', UserInfo <$> user' ?? pwd', path'))


queryComicTable :: (MonadUnliftIO m, MonadThrow m) => SqlBackend -> m ComicTable
queryComicTable sqlBackend = do
    rows <- flip runSqlConn sqlBackend $
        select $ do
            comics :& _urls <-
                from $
#if __GLASGOW_HASKELL__ >= 902
                    table @Comics `InnerJoin` table @Urls
                        `on` (\(comics :& urls) -> comics.comic ==. urls.comic)
            pure (comics.comic, comics.title, comics.folder, comics.volume, comics.chapter, comics.section)
#else
                    Table @Comics `InnerJoin` Table @Urls
                        `on` (\(comics :& urls) -> comics ^. ComicsComic ==. urls ^. UrlsComic)
            pure ( comics ^. ComicsComic, comics ^. ComicsTitle, comics ^. ComicsFolder
                 , comics ^. ComicsVolume, comics ^. ComicsChapter, comics ^. ComicsSection)
#endif
    comics <- L.foldl tryParseRow (return []) rows
    return $ Map.fromList comics
  where
    tryParseRow ::
        (MonadUnliftIO m, MonadThrow m) =>
        m [(Comic, (Title, Path Rel Dir, Volume, Chapter))] ->
        (Value Int, Value Text, Value Text, Value Int, Value Int, Value (Maybe Int)) ->
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
        (Value Int, Value Text, Value Text, Value Int, Value Int, Value (Maybe Int)) ->
        m (Comic, (Title, Path Rel Dir, Volume, Chapter))
    parseRow (comic, title, folder, volume, chapter, section) = do
        folder' <- parseRelDir $ T.unpack $ unValue folder
        return
            ( Comic $ unValue comic
            ,
                ( Title $ unValue title
                , folder'
                , Volume $ unValue volume
                , Chapter (unValue chapter) $ unValue section
                )
            )


queryUrlTable :: (MonadUnliftIO m, MonadThrow m) => SqlBackend -> m UrlTable
queryUrlTable sqlBackend = do
    rows <- flip runSqlConn sqlBackend $
        select $ do
            urls :& webs <-
                from $
#if __GLASGOW_HASKELL__ >= 902
                    table @Urls `InnerJoin` table @Webs
                        `on` (\(urls :& webs) -> urls.web ==. webs.web)
            pure (urls.web, urls.comic, webs.domain, urls.path)
#else
                    Table @Urls `InnerJoin` Table @Webs
                        `on` (\(urls :& webs) -> urls ^. UrlsWeb ==. webs ^. WebsWeb)
            pure ( urls ^. UrlsWeb, urls ^. UrlsComic, webs ^. WebsDomain, urls ^. UrlsPath)
#endif
    urls <- L.foldl tryParseRow (return []) rows
    return $ Map.fromList urls
  where
    tryParseRow ::
        (MonadUnliftIO m, MonadThrow m) =>
        m [(URI, (Web, Comic))] ->
        (Value Int, Value Int, Value Text, Value Text) ->
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
        (Value Int, Value Int, Value Text, Value Text) ->
        m (URI, (Web, Comic))
    parseRow (web, comic, domain, path) = do
        domain' <- mkHost $ unValue domain
        path' <- mkURI $ unValue path
        case toEnumSafely $ unValue web of
            Just web' -> return (https domain' path', (web', Comic $ unValue comic))
            Nothing -> throwM $ InvalidWebNo $ unValue web
