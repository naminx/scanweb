{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Web.MangaGun.Com where

import Import
import Text.Taggy.Lens
import Web.Common
import qualified Web.Common.WeLoveManga as WeLoveManga


newReleaseUrl :: MonadThrow m => Page -> m URI
newReleaseUrl = WeLoveManga.newReleaseUrl


keyElement :: Text
keyElement = WeLoveManga.keyElement


focusComics :: Fold Node (Try (URI, Maybe ReleaseInfo))
focusComics = divThumbItemFlow . liftFold2 (liftA2 (,)) comics relInfo
  where
    divThumbItemFlow = allNamed (only "div") . attributed (hasClasses ["thumb-item-flow", "col-6", "col-md-3"])

    comics :: ToLike Element (Try URI)
    comics = to (view $ divSeriesTitle . anchor . attr "href") . tryParseURI
      where
        divSeriesTitle = allNamed (only "div") . attributed (hasClass "series-title")

    relInfo :: ToLike Element (Try (Maybe ReleaseInfo))
    relInfo =
        to (preview $ divChapterTitle . anchor . contents)
            . tryParseChapter mkChapterNo
            . to (fmap Episode)
            . to (fmap Just)
      where
        divChapterTitle = allNamed (only "div") . attributed (hasClass "chapter-title")
        mkChapterNo = parseEither comicChapter


focusLatestRelInfo :: Fold Node (Try URI)
focusLatestRelInfo = WeLoveManga.focusLatestRelInfo


focusRelInfos :: Fold Node (Try (ReleaseInfo, URI))
focusRelInfos = WeLoveManga.focusRelInfos


focusRelInfo :: Fold Node (Try ReleaseInfo)
focusRelInfo = WeLoveManga.focusRelInfo


focusImages :: Fold Node (Try URI)
focusImages = WeLoveManga.focusImages
