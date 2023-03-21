{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module App.Config
    ( module App.Config
    , chromeDriverExe
    , defaultRootDir
    ) where

#if defined(mingw32_HOST_OS)
import App.Config.Windows
#else
import App.Config.Linux
#endif

import qualified Control.Monad.Script.Http as Http
import Data.Default (Default (..))
import Import hiding (wait)
import Path (Abs, File, SomeBase (Abs), relfile, (</>))
import System.Time.Extra (Seconds)
import Web.Api.WebDriver


connectionString :: Text
connectionString =
    "DRIVER=SQLite;"
        <> "LongNames=0;"
        <> "Timeout=1000;"
        <> "NoTXN=0;"
        <> "SyncPragma=NORMAL;"
        <> "StepAPI=0;"
        <> "Database="


defaultDbFileName :: Path Rel File
defaultDbFileName = [relfile|scanweb.sqlite3|]


defaultDbFile :: Path Abs File
defaultDbFile =
    defaultRootDir </> defaultDbFileName


defaultMaxNumPages :: Int
defaultMaxNumPages = 40


minBoundWeb :: Web
minBoundWeb = Web 3


maxBoundWeb :: Web
maxBoundWeb = Web 5


defaultOptions :: Options
defaultOptions =
    Options
        { _rootDir = defaultRootDir
        , _dbFile = Abs defaultDbFile
        , _appMode = ScanWebs $ Web <$> [unWeb minBoundWeb .. unWeb maxBoundWeb]
        , _maxNumPages = defaultMaxNumPages
        }


chromeConfig :: MonadUnliftIO m => WebDriverConfig m
chromeConfig =
    WDConfig
        { _initialState = defaultWebDriverState
        , _environment = appWebDriverEnvironment
        , _evaluator = liftIO . Http.evalIO evalWDAct
        }
  where
    appWebDriverEnvironment =
        defaultWebDriverEnvironment
            & env .~ appWDEnv
            & logOptions .~ appWDLogOpts
    appWDEnv =
        defaultWDEnv
            & remoteHostname .~ "localhost"
            & remotePort .~ 9515
            & responseFormat .~ SpecFormat
    appWDLogOpts =
        defaultWebDriverLogOptions
            & logSilent .~ True


normalChrome :: Capabilities
normalChrome =
    defaultChromeCapabilities
        & chromeOptions ?~ normalChromeOptions


normalChromeOptions :: ChromeOptions
normalChromeOptions =
    def
        & chromeBinary ?~ chromeExe
        & chromeArgs
            ?~ [ "--user-data-dir=" <> userDataDir
#if MIN_VERSION_GLASGOW_HASKELL(9,2,7,0)
#else
               , "--no-sandbox"
               , "--disable-dev-shm-usage"
#endif
               ]


defaultWaitTime :: Seconds
defaultWaitTime = 0.6


minWaitTime :: Seconds
minWaitTime = 7.0


maxWaitTime :: Seconds
maxWaitTime = 15.0


maxTimeOut :: Seconds
maxTimeOut = 30.0
