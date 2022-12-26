{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Web.MangaRaw.So where

import Import
import qualified RIO.Text as T (pack)
import Replace.Megaparsec (anyTill)
import Text.Megaparsec (eof)
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
keyElement = "a[href='https://mangaraw.so/']"


focusComics :: Fold Node (Try (URI, Maybe ReleaseInfo))
focusComics =
    dropping 1 divSectionContainer
        . divMangaItem
        . liftFold2 (liftA2 (,)) comics relInfo
  where
    divSectionContainer =
        allNamed (only "div")
            . attributed (hasClass "section-container")
    divMangaItem = allNamed (only "div") . attributed (hasClass "manga-item")

    comics :: Fold Element (Try URI)
    comics = taking 1 anchor . to (view $ attr "href") . tryParseURI

    relInfo :: Fold Element (Try (Maybe ReleaseInfo))
    relInfo =
        taking 1 (anchor . attributed (hasClass "text-small"))
            . to (preview contents)
            . tryParseChapter mkChapterNo
            . to (fmap $ Just . Episode)
      where
        mkChapterNo = parseEither $ do
            _ <- string "【第"
            chap <- comicChapter
            _ <- string "話】 "
            return chap


focusLatestRelInfo :: Fold Node (Try URI)
focusLatestRelInfo =
    divChaptersContainer
        . taking 1 (allNamed $ only "tr")
        . taking 1 anchor
        . attr "href"
        . tryParseURI
  where
    divChaptersContainer =
        allNamed (only "div")
            . attributed (hasClass "chapters-container")


focusRelInfos :: Fold Node (Try (ReleaseInfo, URI))
focusRelInfos =
    backwards $
        divChaptersContainer
            . allNamed (only "tr")
            . taking 1 anchor
            . liftFold2 (liftA2 (,)) relInfo url
  where
    divChaptersContainer =
        allNamed (only "div")
            . attributed (hasClass "chapters-container")

    relInfo :: Fold Element (Try ReleaseInfo)
    relInfo = attr "title" . tryParseChapter mkChapterNo . to (fmap Episode)
      where
        mkChapterNo = parseEither $ do
            _ <- anyTill $ string "【第"
            chap <- comicChapter
            _ <- string "話】"
            _ <- anyTill eof
            return chap

    url :: Fold Element (Try URI)
    url = attr "href" . tryParseURI


focusImages :: Fold Node (Try URI)
focusImages =
    divCardWrap . image . url
  where
    divCardWrap = allNamed (only "div") . attributed (hasClass "card-wrap")
    image = elements . named (only "img")
    url = attr "data-src" . tryParseURI
