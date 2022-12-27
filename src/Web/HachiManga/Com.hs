{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Web.HachiManga.Com where

import Import
import Replace.Megaparsec (anyTill)
import Text.Megaparsec (eof)
import Text.Megaparsec.Char (string)
import Text.Taggy.Lens
import Web.Common
import qualified Web.Common.MangaHatachi as MangaHatachi


newReleaseUrl :: MonadThrow m => Page -> m URI
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
focusRelInfo =
    taking 1 selectSingleChapter . optionSelected . relInfo
  where
    selectSingleChapter =
        allNamed (only "select")
            . attributed (hasClass "single-chapter-select")
    optionSelected =
        elements
            . allNamed (only "option")
            . attributed (ix "selected" . only "selected")

    relInfo =
        to (preview contents)
            . tryParseChapter mkChapterNo
            . to (fmap Episode)
      where
        mkChapterNo = parseEither $ do
            _ <- anyTill $ string "第"
            chap <- comicChapter
            _ <- string "話"
            _ <- anyTill eof
            return chap


focusImages :: Fold Node (Try URI)
focusImages = MangaHatachi.focusImages
