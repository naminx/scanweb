{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Web.MangaGohan.Me where

import Import
import qualified RIO.Text as T (pack)
import Text.URI (mkPathPiece)
import Text.URI.Lens (uriPath)
import Text.URI.QQ (uri)
import qualified Web.Common.MangaHatachi as MangaHatachi


newReleaseUrl :: MonadThrow m => Page Int -> m URI
newReleaseUrl (Page n)
    -- This trailing slash is important to flag an ABSOLUTE path.
    | n == 1 = return [uri|/jp1/|]
    | otherwise =
        return $ [uri|/jp1/page/|] & uriPath %~ (<> pageNo)
  where
    pageNo = mkPathPiece $ T.pack $ show n


keyElement :: Text
keyElement = "a.logo"


focusComics :: Fold Node (Try (URI, Maybe ReleaseInfo))
focusComics = MangaHatachi.focusComics


focusLatestRelInfo :: Fold Node (Try URI)
focusLatestRelInfo = MangaHatachi.focusLatestRelInfo


focusRelInfos :: Fold Node (Try (ReleaseInfo, URI))
focusRelInfos = MangaHatachi.focusRelInfos


focusImages :: Fold Node (Try URI)
focusImages = MangaHatachi.focusImages
