{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module App.Config (
    module App.Config,
    defaultRootDir,
) where


#if defined(mingw32_HOST_OS)
import App.Config.Windows
#else
import  App.Config.Linux
#endif

import Data.Default (Default (..))
import Import hiding (wait)
import Path (Abs, File, Path, Rel, SomeBase (Abs), relfile, (</>))
import System.Time.Extra (Seconds)
import Web.Api.WebDriver


timestamp :: IsString s => s
timestamp = __TIMESTAMP__


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
defaultMaxNumPages = 25


defaultOptions :: Options
defaultOptions =
    Options
        { _rootDir = defaultRootDir
        , _dbFile = Abs defaultDbFile
        , _appMode = ScanWebs [minBound :: Web .. maxBound :: Web]
        , _maxNumPages = defaultMaxNumPages
        }


chromeConfig :: WebDriverConfig IO
chromeConfig =
    defaultWebDriverConfig
        & environment .~ appWebDriverEnvironment
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
               , "--save-page-as-mhtml"
               ]


-- This is here to suppress a warning on unused import when call repl from cabal.
_dummy :: Rel
_dummy = undefined


defaultWaitTime :: Seconds
defaultWaitTime = 0.6


minWaitTime :: Seconds
minWaitTime = 7.0


maxWaitTime :: Seconds
maxWaitTime = 15.0
