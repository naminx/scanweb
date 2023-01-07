{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Web.J8Jp.Com where

import qualified Data.Text as T (replace)
import Import
import qualified RIO.Text as T (pack)
import Text.Megaparsec.Char (string)
import Text.Taggy.Lens
import Text.URI (mkPathPiece)
import Text.URI.Lens (uriPath)
import Text.URI.QQ (uri)
import Web.Common


newReleaseUrl :: MonadThrow m => Page -> m URI
newReleaseUrl (Page n)
    -- This trailing slash is important to flag an ABSOLUTE path.
    | n == 1 = return [uri|/|]
    | otherwise =
        return $ [uri|/page/|] & uriPath %~ (<> pageNo)
  where
    pageNo = mkPathPiece $ T.pack $ show n


keyElement :: Text
keyElement = "img[src*='logo.png']"


focusComics :: Fold Node (Try (URI, Maybe ReleaseInfo))
focusComics = divL . liftFold2 (liftA2 (,)) comics relInfo
  where
    divL = allNamed (only "div") . attributed (hasClass "luf")

    comics :: Fold Element (Try URI)
    comics = elements . named (only "a") . attr "href" . tryParseURI

    relInfo :: Fold Element (Try (Maybe ReleaseInfo))
    relInfo =
        elements
            . ulManga
            . taking 1 (to $ preview $ anchor . contents)
            . tryParseChapter mkChapterNo
            . to (fmap $ Just . Episode)
      where
        ulManga = named (only "ul") . attributed (hasClass "Manga")
        mkChapterNo = parseEither $ string "Ch. " >> comicChapter


focusLatestRelInfo :: Fold Node (Try URI)
focusLatestRelInfo =
    divLastEnd . anchor . filteredBy spanEpCurLast . attr "href" . tryParseURI
  where
    divLastEnd = allNamed (only "div") . attributed (hasClass "lastend")
    spanEpCurLast = allNamed (only "span") . attributed (hasClass "epcurlast")


focusRelInfos :: Fold Node (Try (ReleaseInfo, URI))
focusRelInfos =
    divChapterList . backwards (li . liftFold2 (liftA2 (,)) relInfo url)
  where
    divChapterList = allNamed (only "div") . attributed (ix "id" . only "chapterlist")
    li = allNamed (only "li")

    relInfo :: Fold Element (Try ReleaseInfo)
    relInfo = attr "data-num" . tryParseChapter mkChapterNo . to (fmap Episode)
      where
        mkChapterNo = parseEither comicChapter

    url :: Fold Element (Try URI)
    url = allNamed (only "a") . attr "href" . tryParseURI


focusRelInfo :: Fold Node (Try ReleaseInfo)
focusRelInfo =
    taking 1 selectChapter . optionSelected . relInfo
  where
    selectChapter = allNamed (only "select") . attributed (ix "id" . only "chapter")
    optionSelected = allNamed (only "option") . attributed (ix "selected")
    relInfo = to (preview contents) . tryParseChapter mkChapterNo . to (fmap Episode)
      where
        mkChapterNo = parseEither $ string "Chapter " >> comicChapter


focusImages :: Fold Node (Try URI)
focusImages =
    imgTsMainImage . attr "src" . to (fmap $ T.replace " " "%20") . tryParseURI
  where
    imgTsMainImage = allNamed (only "img") . attributed (hasClass "ts-main-image")
