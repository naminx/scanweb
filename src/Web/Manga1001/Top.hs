{-# LANGUAGE NoImplicitPrelude #-}

module Web.Manga1001.Top where

import Import
import qualified Web.Common.Manga1001 as Manga1001


newReleaseUrl :: MonadThrow m => Page Int -> m URI
newReleaseUrl = Manga1001.newReleaseUrl


keyElement :: Text
keyElement = Manga1001.keyElement


focusComics :: Fold Node (Try (URI, Maybe ReleaseInfo))
focusComics = Manga1001.focusComics


focusLatestRelInfo :: Fold Node (Try URI)
focusLatestRelInfo = Manga1001.focusLatestRelInfo


focusRelInfos :: Fold Node (Try (ReleaseInfo, URI))
focusRelInfos = Manga1001.focusRelInfos


focusImages :: Fold Node (Try URI)
focusImages = Manga1001.focusImages
