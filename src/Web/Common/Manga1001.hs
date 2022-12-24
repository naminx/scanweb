{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Web.Common.Manga1001 where

import Import
import qualified RIO.Text as T (pack)
import Replace.Megaparsec (anyTill)
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
keyElement = "a[rel='home']"


focusComics :: Fold Node (Try (URI, Maybe ReleaseInfo))
focusComics =
    h3EntryTitle . liftFold2 (liftA2 (,)) comics noRelInfo
  where
    h3EntryTitle = allNamed (only "h3") . attributed (hasClass "entry-title")

    comics :: ToLike Element (Try URI)
    comics = to (view $ anchor . attr "href") . tryParseURI


focusLatestRelInfo :: Fold Node (Try URI)
focusLatestRelInfo = noLatestRelInfo


focusRelInfos :: Fold Node (Try (ReleaseInfo, URI))
focusRelInfos =
    divChapList . backwards (anchor . liftFold2 (liftA2 (,)) relInfo url)
  where
    divChapList = allNamed (only "div") . attributed (hasClass "chaplist")

    relInfo :: ToLike Element (Try ReleaseInfo)
    relInfo = to (preview contents) . tryParseChapter mkChapterNo . to (fmap Episode)
      where
        mkChapterNo = parseEither $ do
            _ <- anyTill $ string "【第"
            chap <- comicChapter
            _ <- string "話】"
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
