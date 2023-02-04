{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Init where

import App.Config
import Database.Persist.Sqlite hiding (LogFunc)
import Database.Sqlite (open)
import Import
import Path (Abs, File, toFilePath)
import qualified RIO.Map as Map (empty)
import RIO.Process
import qualified RIO.Text as T
import Run
import Text.URI (emptyURI)
import Web.Api.WebDriver (ContextId (..), SessionId, execWebDriverT, newSession)


setupEnv :: forall env s. (HasStateRef s env, HasApp s) => RIO env ()
setupEnv = do
    mkWebTable
    mkComicTable
    mkUrlTable


newWdSession :: (MonadUnliftIO m, MonadThrow m) => m SessionId
newWdSession = do
    (result, _, _) <- liftIO $ execWebDriverT chromeConfig $ newSession normalChrome
    either throwM return result


newSqlBackend :: MonadUnliftIO m => Path Abs File -> m SqlBackend
newSqlBackend dbFilePath = do
    conn <- liftIO $ open filePath
    liftIO $ wrapConnectionInfo (mkSqliteConnectionInfo filePath) conn defaultLogFunc
    where
        filePath = T.pack $ toFilePath dbFilePath
        defaultLogFunc _ _ _ _ = return ()


initApp :: LogFunc -> ProcessContext -> Options -> SqlBackend -> SessionId -> App
initApp logFunction procContext cliOptions sqlBackend wdSess =
    App
        { _logFunc = logFunction
        , _processContext = procContext
        , _options = cliOptions
        , _currentWeb = WeLoMaArt
        , _currentPage = Page 1
        , _currentSqlBackend = sqlBackend
        , _webTable = Map.empty
        , _domainTable = Map.empty
        , _comicTable = Map.empty
        , _urlTable = Map.empty
        , _currentWdSession = wdSess
        , _currentWebInfo = emptyWebInfo
        , _currentComic = Comic 0
        , _currentComicUrl = emptyURI
        , _currentComicInfo = emptyComicInfo
        , _currentComicWindow = ContextId ""
        , _currentReferer = emptyURI
        , _newReleaseInfo = Nothing
        , _stickyLine = ""
        }
