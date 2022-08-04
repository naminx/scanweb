{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Web.MangaRaw.Co where

import App.Chapter
import App.Exceptions
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


newReleaseUrl :: MonadThrow m => Page Int -> m URI
newReleaseUrl (Page n)
    -- This trailing slash is important to flag an ABSOLUTE path.
    | n == 1 = return [uri|/|]
    | otherwise =
        return $ [uri|/page|] & uriPath %~ (<> pageNo)
  where
    pageNo = mkPathPiece $ T.pack $ show n


focusComics :: Fold Node (Try (URI, Maybe ReleaseInfo))
focusComics = anchor . filteredBy h3EntryTitle . liftFold2 (liftA2 (,)) comics noRelInfo
  where
    h3EntryTitle = elements . named (only "h3") . attributed (hasClass "entry-title")

    comics :: Fold Element (Try URI)
    comics = attr "href" . tryParseURI


keyElement :: Text
keyElement = "img.custom-logo[src*='logo.png']"


focusLatestRelInfo :: Fold Node (Try URI)
focusLatestRelInfo = noLatestRelInfo


focusRelInfos :: Fold Node (Try (ReleaseInfo, URI))
focusRelInfos =
    backwards $ anchorTextInfo . liftFold2 (liftA2 (,)) relInfo url
  where
    anchorTextInfo :: Fold Node Element
    anchorTextInfo = anchor . attributed (hasClass "text-info")

    relInfo :: Fold Element (Try ReleaseInfo)
    relInfo = attr "title" . tryParseRelInfo
      where
        tryParseRelInfo = to (maybeToTry ChapterNoNotFound) . to (>>= mkChapterNo)
          where
            mkChapterNo = parseEither $ do
                _ <- anyTill $ string "【第"
                chap <- comicChapter
                _ <- string "話】"
                _ <- anyTill eof
                return $ Episode chap

    url :: Fold Element (Try URI)
    url = attr "href" . tryParseURI


focusImages :: Fold Node (Try URI)
focusImages =
    divCardWrap . img . attr "data-src" . tryParseURI
  where
    divCardWrap = allNamed (only "div") . attributed (hasClass "card-wrap")
    img = elements . named (only "img")
