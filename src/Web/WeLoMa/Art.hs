{-# LANGUAGE NoImplicitPrelude #-}

module Web.WeLoMa.Art where

import Import
import qualified Web.Common.WeLoveManga as WeLoveManga


newReleaseUrl :: MonadThrow m => Page Int -> m URI
newReleaseUrl = WeLoveManga.newReleaseUrl


keyElement :: Text
keyElement = WeLoveManga.keyElement


focusComics :: Fold Node (Try (URI, Maybe ReleaseInfo))
focusComics = WeLoveManga.focusComics


focusLatestRelInfo :: Fold Node (Try URI)
focusLatestRelInfo = WeLoveManga.focusLatestRelInfo


focusRelInfos :: Fold Node (Try (ReleaseInfo, URI))
focusRelInfos = WeLoveManga.focusRelInfos


focusRelInfo :: Fold Node (Try ReleaseInfo)
focusRelInfo = WeLoveManga.focusRelInfo


focusImages :: Fold Node (Try URI)
focusImages = WeLoveManga.focusImages
