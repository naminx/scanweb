{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Web.WeLoveManga.One where

import App.Chapter
import Import
import Text.Megaparsec.Char (string)
import Text.Taggy.Lens
import Web.Common
import qualified Web.Common.WeLoveManga as WeLoveManga


newReleaseUrl :: MonadThrow m => Page Int -> m URI
newReleaseUrl = WeLoveManga.newReleaseUrl


keyElement :: Text
keyElement = WeLoveManga.keyElement


focusComics :: Fold Node (Try (URI, Maybe ReleaseInfo))
focusComics = divThumbItemFlow . liftFold2 (liftA2 (,)) comics relInfo
  where
    divThumbItemFlow = allNamed (only "div") . attributed (hasClasses ["thumb-item-flow", "col-6", "col-md-3"])

    comics :: ToLike Element (Try URI)
    comics = to (view $ divComicClass . anchor . attr "href") . tryParseURI
      where
        divComicClass = allNamed (only "div") . attributed (hasClasses ["thumb_attr", "series-title"])

    relInfo :: ToLike Element (Try (Maybe ReleaseInfo))
    relInfo =
        to (view $ divChapterTitle . attr "title")
            . tryParseChapter mkChapterNo
            . to (fmap Episode)
            . to (fmap Just)
      where
        divChapterTitle = allNamed (only "div") . attributed (hasClass "chapter-title")
        mkChapterNo = parseEither $ string "Chap " >> comicChapter


focusLatestRelInfo :: Fold Node (Try URI)
focusLatestRelInfo = WeLoveManga.focusLatestRelInfo


focusRelInfos :: Fold Node (Try (ReleaseInfo, URI))
focusRelInfos = WeLoveManga.focusRelInfos


focusRelInfo :: Fold Node (Try ReleaseInfo)
focusRelInfo = WeLoveManga.focusRelInfo


focusImages :: Fold Node (Try URI)
focusImages = WeLoveManga.focusImages
