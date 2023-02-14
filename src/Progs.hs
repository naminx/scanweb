{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Progs where

import App.Exceptions
import App.Types
import Control.Lens hiding (elements)
import Formatting (format)
import Formatting.Combinators (lpadded)
import Formatting.Formatters (int)
import GHC.Exts (sortWith)
import Import
import qualified RIO.Map as Map (toList)
import RIO.Process (HasProcessContext (..))
import qualified RIO.Text as T (pack)
import qualified RIO.Text.Lazy as TL (toStrict)
import Run
import System.Console.ANSI (Color (..))
import Text.Pretty.Simple (pPrint)
import Text.URI (renderStr, unRText)


-- This function is here only to suppress warnings.
_dummy :: Void
_dummy =
    undefined
        (pPrint (0 :: Int) :: IO ())


progScanWebs
    :: forall env s
     . (HasStateRef s env, HasApp s, HasLogFunc s, HasProcessContext s)
    => [Web]
    -> RIO env ()
progScanWebs =
    traverse_ $
        [ scanWeb
        --      , testComics
        --      , testChapter
        ]
            ^?! ix 0


progUpdateComic
    :: forall env s
     . (HasStateRef s env, HasApp s, HasLogFunc s, HasProcessContext s)
    => (Web, Comic, Maybe ReleaseInfo)
    -> RIO env ()
progUpdateComic (web, comic, relInfo) = do
    setWebTo web
    setComicTo comic
    newReleaseInfo .= relInfo
    result <- tryAny openAndScanComic
    case result of
        Left someException -> printException someException
        Right Nothing -> do
            (title, _, _, _) <- currentComicInfo <%= id
            runSimpleApp $ logInfo $ display $ vivid Green <> unTitle title <> " is up to date"
        Right (Just chapter) -> do
            updateComicTable comic chapter


progDownloadRelease
    :: forall env s
     . (HasStateRef s env, HasApp s, HasLogFunc s, HasProcessContext s)
    => (Web, Comic, ReleaseInfo)
    -> RIO env ()
progDownloadRelease (web, comic, relInfo) = do
    setWebTo web
    setComicTo comic
    result <- tryAny $ openAndDownloadRelease relInfo
    case result of
        Left someException -> printException someException
        Right _ -> return ()


progDownloadChapter
    :: forall env s
     . (HasStateRef s env, HasApp s, HasLogFunc s, HasProcessContext s)
    => URI
    -> RIO env ()
progDownloadChapter url = do
    web <- urlToWeb url >>= maybe (throwM $ UnknownWeb $ renderStr url) return
    setWebTo web
    result <- tryAny $ openAndDownloadAddress url
    case result of
        Left someException -> printException someException
        Right _ -> return ()


progListWebs :: forall env s. (HasStateRef s env, HasApp s) => RIO env ()
progListWebs = do
    webTab <- webTable <%= id
    printWebs webTab
  where
    printWebs =
        traverse_ printWeb . sortWith (\(web, _) -> unWeb web) . Map.toList

    printWeb (web, wInfo) =
        runSimpleApp . logInfo . display $
            vivid Yellow
                <> TL.toStrict (format (lpadded 2 ' ' int) $ unWeb web)
                <> (resetSGR <> ") ")
                <> (vivid Green <> unRText (wInfo ^. webDomain))
                <> resetSGR


progListComics :: forall env s. (HasStateRef s env, HasApp s) => RIO env ()
progListComics = do
    comicTab <- comicTable <%= id
    printComics comicTab
  where
    printComics =
        traverse_ printComic . Map.toList

    printComic (Comic {unComic = comic}, (Title {unTitle = title}, _, Volume vol, chap)) =
        runSimpleApp . logInfo . display $
            vivid Yellow
                <> TL.toStrict (format (lpadded 3 ' ' int) comic)
                <> (resetSGR <> ") ")
                <> (vivid Green <> title)
                <> ( vivid Black
                        <> " (Vol."
                        <> T.pack (show vol)
                        <> (", Ch." <> T.pack (show chap) <> ")")
                   )
                <> resetSGR
