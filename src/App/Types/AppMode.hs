{-# LANGUAGE NoImplicitPrelude #-}

module App.Types.AppMode where

import App.Types.Comic
import App.Types.ReleaseInfo
import App.Types.Web
import Lib


data AppMode
    = ScanWebs [Web]
    | UpdateComic (Web, Comic, Maybe ReleaseInfo)
    | DownloadRelease (Web, Comic, ReleaseInfo)
    | ListWebs
    | ListComics
    deriving (Eq, Show)
