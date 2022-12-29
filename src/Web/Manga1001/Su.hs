{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Web.Manga1001.Su where

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
focusComics = Manga1001.focusComics


focusLatestRelInfo :: Fold Node (Try URI)
focusLatestRelInfo = noLatestRelInfo


focusRelInfos :: Fold Node (Try (ReleaseInfo, URI))
focusRelInfos =
    backwards (anchorChapterColor . liftFold2 (liftA2 (,)) relInfo url)
  where
    anchorChapterColor = anchor . attributed (hasClass "chapter-color")

    relInfo :: ToLike Element (Try ReleaseInfo)
    relInfo =
        to (view $ anchor . attr "title")
            . tryParseChapter mkChapterNo
            . to (fmap Episode)
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
    taking 1 divChapterList2 . optionSelected . relInfo
  where
    divChapterList2 =
        allNamed (only "div") . attributed (hasClass "chapterlist2")
    optionSelected = allNamed (only "option") . attributed (ix "selected")
    relInfo =
        to (preview contents)
            . tryParseChapter mkChapterNo
            . to (fmap Episode)
      where
        mkChapterNo = parseEither $ do
            chap <- comicChapter
            _ <- string "話"
            _ <- anyTill eof
            return chap


focusImages :: Fold Node (Try URI)
focusImages =
    figureWpBlockImage . image . url
  where
    figureWpBlockImage =
        allNamed (only "figure") . attributed (hasClass "wp-block-image")
    image = elements . named (only "img")
    url = attr "data-src" . tryParseURI
