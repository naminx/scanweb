{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Web.KlManga.Net where

import App.Chapter
import Import
import Text.Megaparsec.Char (string)
import Text.Taggy.Lens
import Text.URI.QQ (uri)
import Web.Common
import qualified Web.Common.WeLoveManga as WeLoveManga


newReleaseUrl :: MonadThrow m => Page Int -> m URI
newReleaseUrl (Page n)
    | n == 1 = return [uri|/manga-list.html?listType=pagination&sort=last_update&sort_type=DESC|]
    | otherwise = WeLoveManga.newReleaseUrl $ Page n


keyElement :: Text
keyElement = "a.navbar-brand"


focusComics :: Fold Node (Try (URI, Maybe ReleaseInfo))
focusComics = divThumbItemFlow . liftFold2 (liftA2 (,)) comics relInfo
  where
    divThumbItemFlow = allNamed (only "div") . attributed (hasClasses ["thumb-item-flow", "col-6", "col-md-3"])

    comics :: ToLike Element (Try URI)
    comics = to (view $ divSeriesTitle . anchor . attr "href") . tryParseURI
      where
        divSeriesTitle = allNamed (only "div") . attributed (hasClass "series-title")

    relInfo :: ToLike Element (Try (Maybe ReleaseInfo))
    relInfo =
        to (preview $ divChapterTitle . anchor . contents)
            . tryParseChapter mkChapterNo
            . to (fmap Episode)
            . to (fmap Just)
      where
        divChapterTitle = allNamed (only "div") . attributed (hasClass "chapter-title")
        mkChapterNo = parseEither $ string "Last chapter: " >> comicChapter


focusLatestRelInfo :: Fold Node (Try URI)
focusLatestRelInfo = WeLoveManga.focusLatestRelInfo' "Lastest chapter"


focusRelInfos :: Fold Node (Try (ReleaseInfo, URI))
focusRelInfos =
    divTabChapper . backwards (anchorChapter . liftFold2 (liftA2 (,)) relInfo url)
  where
    divTabChapper = allNamed (only "div") . attributed (ix "id" . only "tab-chapper")
    anchorChapter = anchor . attributed (hasClass "chapter")

    relInfo :: ToLike Element (Try ReleaseInfo)
    relInfo = to (preview $ allNamed (only "b") . contents) . tryParseChapter mkChapterNo . to (fmap Episode)
      where
        mkChapterNo = parseEither $ string "Chapter " >> comicChapter

    url :: Fold Element (Try URI)
    url = attr "href" . tryParseURI


focusRelInfo :: Fold Node (Try ReleaseInfo)
focusRelInfo = WeLoveManga.focusRelInfo


focusImages :: Fold Node (Try URI)
focusImages = WeLoveManga.focusImages
