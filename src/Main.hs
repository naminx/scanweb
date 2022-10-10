{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import App.Chapter
import App.Config
import App.Types
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import Data.Tuple.Extra (dupe)
import Database.Esqueleto.Experimental hiding ((<&>), (^.))
import qualified Database.Esqueleto.Experimental as ES
import Import hiding (from, on)
import Init
import Options
import Path (SomeBase (..), (</>))
import Progs
import RIO.Process
import RIO.State
import qualified RIO.Text as T
import qualified RIO.Text.Lazy as TL
import Replace.Megaparsec (anyTill, breakCap)
import Run (runWdSession, xmlHttpRequest)
import System.Console.ANSI (SGR (..), setSGR)
import System.Console.ANSI.Types (Color (..))
import System.Process (CreateProcess (..), StdStream (..), withCreateProcess)
import qualified System.Process as P
import System.Time.Extra (sleep)
import Text.Megaparsec (Parsec, ShowErrorComponent (..), eof, many, parseTest, some, try)
import Text.Megaparsec.Char (space1, string)
import Text.Pretty.Simple (pPrint)
import Text.RawString.QQ (r)
import Text.Taggy.Lens
import Text.URI (render)
import Text.URI.QQ (uri)
import Web.Api.WebDriver as WD


-- This function is here only to suppress warnings.
_dummy :: Void
_dummy =
    undefined
        Data.Tuple.Extra.dupe
        ("" ^.. Text.Taggy.Lens.html)
        (pPrint (0 :: Int) :: IO ())
        [uri||]
        xmlHttpRequest


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
                    then withCreateProcess chromedriver_proc $ \_ mb_hout _ _ -> case mb_hout of
                        Just hout -> do
                            waitForChromeDriver hout
                            bracket newWdSession (`runWdSession` deleteSession) $
                                \wdSess -> do
                                    myApp <-
                                        newSomeRef $
                                            initApp logFunction procContext cliOptions conn wdSess
                                    runRIO myApp $ do
                                        setupEnv
                                        getProg $ cliOptions ^. appMode
                        Nothing -> return ()
                    else do
                        myApp <-
                            newSomeRef $
                                initApp logFunction procContext cliOptions conn undefined
                        runRIO myApp $ do
                            setupEnv
                            getProg $ cliOptions ^. appMode
  where
    chromedriver_proc = (P.proc "/run/current-system/sw/bin/chromedriver" []) {std_out = CreatePipe}
    waitForChromeDriver hout = do
        response <- T.hGetLine hout
        if response == "ChromeDriver was started successfully."
            then return ()
            else waitForChromeDriver hout


getProg ::
    forall env s.
    (HasStateRef s env, HasApp s, HasLogFunc s, HasProcessContext s) =>
    AppMode ->
    RIO env ()
getProg appmode = case appmode of
    ScanWebs webs -> progScanWebs webs
    UpdateComic (web, comic, relInfo) -> progUpdateComic (web, comic, relInfo)
    DownloadRelease (web, comic, relInfo) -> progDownloadRelease (web, comic, relInfo)
    ListWebs -> progListWebs
    ListComics -> progListComics
