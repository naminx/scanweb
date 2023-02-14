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

import App.Config
import App.Tables
import App.Types
import Control.Monad.Extra (whileM)
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Data.Aeson.Lens
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
import Text.URI (Authority (Authority), mkURI, render, unRText)
import Text.URI.Lens (authHost, uriAuthority)
import Text.URI.QQ (host, uri)
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
                                withAsync (consumeInput hout)
                                    $ const
                                    $ bracket
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


getProg
    :: forall env s
     . (HasStateRef s env, HasApp s, HasLogFunc s, HasProcessContext s)
    => AppMode
    -> RIO env ()
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
    (+||+) = unsafeSqlBinOp " || "
    query = select $ do
        webs :& urls :& comics <-
            from
                $ ( table @Webs `InnerJoin` table @Urls
                        `on` (\(webs :& urls) -> webs.web ==. urls.web)
                  )
                    `InnerJoin` table @Comics
                `on` (\(_ :& urls :& comics) -> urls.comic ==. comics.comic)
        let fullUrl = val [uri|https://|] +||+ webs.domain +||+ urls.path
        where_ $ fullUrl `in_` valList links
        orderBy $ map (asc . (fullUrl !=.) . val) links
        pure
            ( urls.comic
            , fullUrl
            , (comics.title, comics.folder, comics.volume, comics.chapter)
            )
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
    query = select $ do
        comics <- from $ table @Comics
        where_ $ comics.comic ==. val (Comic c)
        pure (comics.title, comics.folder, comics.volume, comics.chapter)
    unValues (title, path, volume, chapter) =
        (unValue title, unValue path, unValue volume, unValue chapter)


queryWeb :: MonadUnliftIO m => URI -> m (Maybe Webs)
queryWeb url =
    bracket (newSqlBackend defaultDbFile) (liftIO . close') $
        (fmap unValues . preview _head <$>) . runSqlConn query
  where
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
    unValues
        :: ( Value Web
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
           )
        -> Webs
    unValues (w, d, u, p, a, t1, t2, t3, t4, t5, t6) =
        Webs
            (unValue w)
            (unValue d)
            (unValue u)
            (unValue p)
            (unValue a)
            (unValue t1)
            (unValue t2)
            (unValue t3)
            (unValue t4)
            (unValue t5)
            (unValue t6)
