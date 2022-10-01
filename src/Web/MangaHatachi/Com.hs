{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Web.MangaHatachi.Com where

import Import
import qualified Web.Common.MangaHatachi as MangaHatachi

newReleaseUrl :: MonadThrow m => Page Int -> m URI
newReleaseUrl = MangaHatachi.newReleaseUrl

keyElement :: Text
keyElement = "img.img-responsive[src*='logo.png']"

focusComics :: Fold Node (Try (URI, Maybe ReleaseInfo))
focusComics = MangaHatachi.focusComics

focusLatestRelInfo :: Fold Node (Try URI)
focusLatestRelInfo = MangaHatachi.focusLatestRelInfo

focusRelInfos :: Fold Node (Try (ReleaseInfo, URI))
focusRelInfos = MangaHatachi.focusRelInfos

focusRelInfo :: Fold Node (Try ReleaseInfo)
focusRelInfo = MangaHatachi.focusRelInfo

focusImages :: Fold Node (Try URI)
focusImages = MangaHatachi.focusImages
