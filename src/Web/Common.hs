{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Web.Common where

import App.Exceptions
import Import
import Text.Taggy.Lens
import Text.URI (emptyURI, mkURI)


tryParseChapter :: (Text -> Try Chapter) -> ToLike (Maybe Text) (Try Chapter)
tryParseChapter mkChapterNo = to $ maybeToTry ChapterNoNotFound >>> (>>= mkChapterNo)


tryParseURI :: ToLike (Maybe Text) (Try URI)
tryParseURI = to $ maybeToTry ChapterLinkNotFound >>> (>>= mkURI)


noLatestRelInfo :: Fold Node (Try URI)
noLatestRelInfo = to (const $ Right emptyURI) . filtered (const False)


noRelInfo :: ToLike Element (Try (Maybe ReleaseInfo))
noRelInfo = to $ const $ Right Nothing


anchor :: HasElement a => Fold a Element
anchor = allNamed $ only "a"
