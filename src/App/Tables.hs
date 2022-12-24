{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

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
            pure (webs.web, webs.domain, webs.username, webs.password, webs.sentinel)
    webs <- L.foldl tryParseRow (return []) rows
    return $ Map.fromList webs
  where
    tryParseRow
        :: (MonadUnliftIO m, MonadThrow m)
        => m [(Web, WebInfo)]
        -> (Value Web, Value Domain, Value (Maybe (RText 'Username)), Value (Maybe (RText 'Password)), Value URI)
        -> m [(Web, WebInfo)]
    tryParseRow acc args = do
        result <- tryAny $ parseRow args
        case result of
            Right row -> acc <&> (<> [row])
            Left someException -> do
                runSimpleApp . logError . display $
                    vivid Red <> T.pack (displayException someException)
                acc

    parseRow
        :: MonadThrow m
        => (Value Web, Value Domain, Value (Maybe (RText 'Username)), Value (Maybe (RText 'Password)), Value URI)
        -> m (Web, WebInfo)
    parseRow (web, domain, user, pwd, path) = do
        return (unValue web, (unValue domain, UserInfo <$> unValue user ?? unValue pwd, unValue path))


queryComicTable :: (MonadUnliftIO m, MonadThrow m) => SqlBackend -> m ComicTable
queryComicTable sqlBackend = do
    rows <- runSql sqlBackend $
        select $ do
            comics :& _urls <-
                from
                    $ table @Comics `InnerJoin` table @Urls
                    `on` (\(comics :& urls) -> comics.comic ==. urls.comic)
            pure (comics.comic, comics.title, comics.folder, comics.volume, comics.chapter)
    comics <- L.foldl tryParseRow (return []) rows
    return $ Map.fromList comics
  where
    tryParseRow
        :: (MonadUnliftIO m, MonadThrow m)
        => m [(Comic, (Title, Path Rel Dir, Volume, Chapter))]
        -> (Value Comic, Value Title, Value (Path Rel Dir), Value Volume, Value Chapter)
        -> m [(Comic, (Title, Path Rel Dir, Volume, Chapter))]
    tryParseRow acc args = do
        result <- tryAny $ parseRow args
        case result of
            Right row -> acc <&> (<> [row])
            Left someException -> do
                runSimpleApp . logError . display $
                    vivid Red <> T.pack (displayException someException)
                acc

    parseRow
        :: MonadThrow m
        => (Value Comic, Value Title, Value (Path Rel Dir), Value Volume, Value Chapter)
        -> m (Comic, (Title, Path Rel Dir, Volume, Chapter))
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
        select $ do
            urls :& webs <-
                from
                    $ table @Urls `InnerJoin` table @Webs
                    `on` (\(urls :& webs) -> urls.web ==. webs.web)
            pure (urls.web, urls.comic, webs.domain, urls.path)
    urls <- L.foldl tryParseRow (return []) rows
    return $ Map.fromList urls
  where
    tryParseRow
        :: (MonadUnliftIO m, MonadThrow m)
        => m [(URI, (Web, Comic))]
        -> (Value Web, Value Comic, Value Domain, Value URI)
        -> m [(URI, (Web, Comic))]
    tryParseRow acc args = do
        result <- tryAny $ parseRow args
        case result of
            Right row -> acc <&> (<> [row])
            Left someException -> do
                runSimpleApp . logError . display $
                    vivid Red <> T.pack (displayException someException)
                acc

    parseRow
        :: MonadThrow m
        => (Value Web, Value Comic, Value Domain, Value URI)
        -> m (URI, (Web, Comic))
    parseRow (web, comic, domain, path) = do
        return (https (unValue domain) (unValue path), (unValue web, unValue comic))
