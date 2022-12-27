{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Web.Manga1001.In where

import Import
import Replace.Megaparsec (anyTill)
import Text.Megaparsec (eof)
import Text.Megaparsec.Char (string)
import Text.Taggy.Lens
import Web.Common
import qualified Web.Common.Manga1001 as Manga1001


newReleaseUrl :: MonadThrow m => Page -> m URI
newReleaseUrl = Manga1001.newReleaseUrl


keyElement :: Text
keyElement = Manga1001.keyElement


focusComics :: Fold Node (Try (URI, Maybe ReleaseInfo))
focusComics = divRotateImg . liftFold2 (liftA2 (,)) comics noRelInfo
  where
    divRotateImg = allNamed (only "div") . attributed (hasClass "rotate-img")

    comics :: ToLike Element (Try URI)
    comics = to (view $ taking 1 anchor . attr "href") . tryParseURI


focusLatestRelInfo :: Fold Node (Try URI)
focusLatestRelInfo = noLatestRelInfo


focusRelInfos :: Fold Node (Try (ReleaseInfo, URI))
focusRelInfos =
    divListScroll . backwards (anchor . liftFold2 (liftA2 (,)) relInfo url)
  where
    divListScroll = allNamed (only "div") . attributed (hasClass "list-scoll")

    relInfo :: ToLike Element (Try ReleaseInfo)
    relInfo = to (view $ anchor . attr "title") . tryParseChapter mkChapterNo . to (fmap Episode)
      where
        mkChapterNo = parseEither $ do
            _ <- anyTill $ string "【第"
            chap <- comicChapter
            _ <- string "話】"
            return chap

    url :: Fold Element (Try URI)
    url = attr "href" . tryParseURI


focusRelInfo :: Fold Node (Try ReleaseInfo)
focusRelInfo =
    taking 1 divFormGroup . optionSelected . relInfo
  where
    divFormGroup = allNamed (only "div") . attributed (hasClass "form-group")
    optionSelected = allNamed (only "option") . attributed (ix "selected")
    relInfo = to (preview contents) . tryParseChapter mkChapterNo . to (fmap Episode)
      where
        mkChapterNo = parseEither $ do
            _ <- string "【第"
            chap <- comicChapter
            _ <- string "話】"
            _ <- anyTill eof
            return chap


focusImages :: Fold Node (Try URI)
focusImages = Manga1001.focusImages
