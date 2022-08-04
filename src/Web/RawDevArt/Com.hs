{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Web.RawDevArt.Com where

import App.Chapter
import Import
import qualified RIO.Text as T (pack)
import Replace.Megaparsec (anyTill)
import Text.Megaparsec (eof)
import Text.Megaparsec.Char (string)
import Text.Taggy.Lens
import Text.URI (mkQueryValue)
import Text.URI.Lens (queryParam, uriQuery)
import Text.URI.QQ (queryKey, uri)
import Web.Common


newReleaseUrl :: MonadThrow m => Page Int -> m URI
newReleaseUrl (Page n)
    -- This trailing slash is important to flag an ABSOLUTE path.
    | n == 1 = return [uri|/|]
    | otherwise = do
        pageNo <- mkQueryValue $ T.pack $ show n
        return $
            [uri|/?page=|]
                & uriQuery . queryParam [queryKey|page|] .~ pageNo


keyElement :: Text
keyElement = "img.img-fluid[src*='rawdevart-1bw-s.png']"


focusComics :: Fold Node (Try (URI, Maybe ReleaseInfo))
focusComics = mainHome . divOverlay . liftFold2 (liftA2 (,)) comics relInfo
  where
    mainHome = allNamed (only "main") . attributed (ix "id" . only "main-home")
    divOverlay = allNamed (only "div") . attributed (hasClass "overlay")

    comics :: Fold Element (Try URI)
    comics = taking 1 anchorHead . attr "href" . tryParseURI
      where
        anchorHead = allNamed (only "a") . attributed (hasClass "head")

    relInfo :: ToLike Element (Try (Maybe ReleaseInfo))
    relInfo =
        to (preview $ paraInfo . span_ . contents)
            . tryParseChapter mkChapterNo
            . to (fmap $ Just . Episode)
      where
        paraInfo = allNamed (only "p") . attributed (hasClass "info")
        span_ = allNamed (only "span")
        mkChapterNo = parseEither $ string "Chapter " >> comicChapter


focusLatestRelInfo :: Fold Node (Try URI)
focusLatestRelInfo =
    divLastEnd . anchor . filteredBy spanEpCurLast . attr "href" . tryParseURI
  where
    divLastEnd = allNamed (only "div") . attributed (hasClass "lastend")
    spanEpCurLast = allNamed (only "span") . attributed (hasClass "epcurlast")


focusRelInfos :: Fold Node (Try (ReleaseInfo, URI))
focusRelInfos =
    backwards $ divListGroupItem . anchor . liftFold2 (liftA2 (,)) relInfo url
  where
    divListGroupItem = allNamed (only "div") . attributed (hasClass "list-group-item")

    relInfo :: Fold Element (Try ReleaseInfo)
    relInfo = attr "title" . tryParseChapter mkChapterNo . to (fmap Episode)
      where
        mkChapterNo = parseEither $ string "Chapter " >> comicChapter

    url :: Fold Element (Try URI)
    url = attr "href" . tryParseURI


focusRelInfo :: Fold Node (Try ReleaseInfo)
focusRelInfo =
    allNamed (only "title") . relInfo
  where
    relInfo = to (preview contents) . tryParseChapter mkChapterNo . to (fmap Episode)
      where
        mkChapterNo = parseEither $ do
            _ <- string "Chapter "
            chap <- comicChapter
            _ <- string " | "
            _ <- anyTill eof
            return chap


focusImages :: Fold Node (Try URI)
focusImages =
    imgImgFluidNotLasy . attr "src" . tryParseURI
  where
    imgImgFluidNotLasy = allNamed (only "img") . attributed (hasClasses ["img-fluid", "not-lazy"])
