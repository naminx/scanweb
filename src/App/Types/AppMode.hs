{-# LANGUAGE NoImplicitPrelude #-}

module App.Types.AppMode where

import App.Types.Comic
import App.Types.ReleaseInfo
import App.Types.Web
import Lib
import Text.URI (URI)


data AppMode
    = ScanWebs [Web]
    | UpdateComic (Web, Comic, Maybe ReleaseInfo)
    | DownloadRelease (Web, Comic, ReleaseInfo)
    | DownloadAddress URI
    | ListWebs
    | ListComics
    deriving (Eq, Show)
