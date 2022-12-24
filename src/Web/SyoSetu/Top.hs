{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Web.SyoSetu.Top where

import Import
import Replace.Megaparsec (anyTill)
import Text.Megaparsec.Char (string)
import Text.Taggy.Lens
import Text.URI.Lens (uriTrailingSlash)
import Web.Common
import qualified Web.Common.Manga1001 as Manga1001


newReleaseUrl :: MonadThrow m => Page -> m URI
newReleaseUrl = fmap (set uriTrailingSlash False) . Manga1001.newReleaseUrl


keyElement :: Text
keyElement = Manga1001.keyElement


focusComics :: Fold Node (Try (URI, Maybe ReleaseInfo))
focusComics = Manga1001.focusComics


focusLatestRelInfo :: Fold Node (Try URI)
focusLatestRelInfo = Manga1001.focusLatestRelInfo


focusRelInfos :: Fold Node (Try (ReleaseInfo, URI))
focusRelInfos =
    divChapList . backwards (anchor . liftFold2 (liftA2 (,)) relInfo url)
  where
    divChapList = allNamed (only "div") . attributed (hasClass "chaplist")

    relInfo :: ToLike Element (Try ReleaseInfo)
    relInfo = to (preview contents) . tryParseChapter mkChapterNo . to (fmap Episode)
      where
        mkChapterNo = parseEither $ do
            _ <- anyTill $ string " - Chapter "
            comicChapter

    url :: Fold Element (Try URI)
    url = attr "href" . tryParseURI


focusRelInfo :: Fold Node (Try ReleaseInfo)
focusRelInfo =
    taking 1 divChapList . optionSelected . relInfo
  where
    divChapList = allNamed (only "div") . attributed (hasClass "chaplist")
    optionSelected = allNamed (only "option") . attributed (ix "selected")
    relInfo = to (preview contents) . tryParseChapter mkChapterNo . to (fmap Episode)
      where
        mkChapterNo = parseEither $ do
            _ <- anyTill $ string " - Chapter "
            comicChapter


focusImages :: Fold Node (Try URI)
focusImages = Manga1001.focusImages
