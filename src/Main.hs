{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import App.Chapter
import App.Config
import App.Tables
import App.Types
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import Data.Tuple.Extra (dupe)
import Database.Esqueleto.Experimental hiding ((<&>), (^.))
import qualified Database.Esqueleto.Experimental as ES
import Database.Esqueleto.Internal.Internal (unsafeSqlBinOp)
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
import Text.URI (Authority (Authority), mkURI, render, unRText)
import Text.URI.Lens (authHost, uriAuthority)
import Text.URI.QQ (host, uri)
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


getProg
    :: forall env s
     . (HasStateRef s env, HasApp s, HasLogFunc s, HasProcessContext s)
    => AppMode
    -> RIO env ()
getProg appmode = case appmode of
    ScanWebs webs -> progScanWebs webs
    UpdateComic (web, comic, relInfo) -> progUpdateComic (web, comic, relInfo)
    DownloadRelease (web, comic, relInfo) -> progDownloadRelease (web, comic, relInfo)
    ListWebs -> progListWebs
    ListComics -> progListComics


queryComics :: MonadUnliftIO m => [URI] -> m [(Comic, URI)]
queryComics [] = return []
queryComics links =
    bracket (newSqlBackend defaultDbFile) (liftIO . close') $
        (map (bimap unValue unValue) <$>) . runSqlConn query
  where
    (+||+) = unsafeSqlBinOp " || "
    query = select $ do
        webs :& urls :& _comics <-
            {- HLINT ignore "Fuse on/on" -}
            from
                $ table @Webs `InnerJoin` table @Urls
                `on` (\(webs :& urls) -> webs.web ==. urls.web) `InnerJoin` table @Comics
                `on` (\(_ :& urls :& comics) -> urls.comic ==. comics.comic)
        let fullUrl = val [uri|https://|] +||+ webs.domain +||+ urls.path
        where_ $ fullUrl `in_` valList links
        orderBy $ map (asc . (fullUrl !=.) . val) links
        pure (urls.comic, fullUrl)
