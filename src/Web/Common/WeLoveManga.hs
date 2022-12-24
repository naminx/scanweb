{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Web.Common.WeLoveManga where

import Data.Char (isSpace)
import Import
import qualified RIO.Text as T
import Text.Megaparsec.Char (string)
import Text.Taggy.Lens
import Text.URI (mkQueryValue)
import Text.URI.Lens (queryParam, uriQuery)
import Text.URI.QQ (queryKey, uri)
import Web.Common


-- import Debug.Trace

newReleaseUrl :: MonadThrow m => Page -> m URI
newReleaseUrl (Page n)
    | n == 1 = return [uri|/manga-list.html?sort=last_update|]
    | otherwise = do
        pageNo <- mkQueryValue $ T.pack $ show n
        return $
            [uri|/manga-list.html?listType=pagination&page=&artist=&author=&group=&m_status=&name=&genre=&ungenre=&magazine=&sort=last_update&sort_type=DESC|]
                & uriQuery . queryParam [queryKey|page|] .~ pageNo


keyElement :: Text
keyElement = "a.navbar-brand[href='/']"


focusComics :: Fold Node (Try (URI, Maybe ReleaseInfo))
focusComics = focusComicsAux "Chap "


focusComicsAux :: Text -> Fold Node (Try (URI, Maybe ReleaseInfo))
focusComicsAux chapterLabel = divThumbItemFlow . liftFold2 (liftA2 (,)) comics relInfo
  where
    divThumbItemFlow = allNamed (only "div") . attributed (hasClasses ["thumb-item-flow", "col-6", "col-md-3"])

    comics :: ToLike Element (Try URI)
    comics = to (view $ divComicClass . anchor . attr "href") . tryParseURI
      where
        divComicClass = allNamed (only "div") . attributed (hasClass "thumb-wrapper")

    relInfo :: ToLike Element (Try (Maybe ReleaseInfo))
    relInfo =
        to (view $ divChapterTitle . attr "title")
            . tryParseChapter mkChapterNo
            . to (fmap Episode)
            . to (fmap Just)
      where
        divChapterTitle = allNamed (only "div") . attributed (hasClass "chapter-title")
        mkChapterNo = parseEither $ string chapterLabel >> comicChapter


focusLatestRelInfo :: Fold Node (Try URI)
focusLatestRelInfo = focusLatestRelInfo' "Read the last chapter"


focusLatestRelInfo' :: Text -> Fold Node (Try URI)
focusLatestRelInfo' latestLabel =
    anchorBtnDanger . filterLastChap . hrefToURI
  where
    anchorBtnDanger = anchor . attributed (hasClass "btn-danger")
    filterLastChap = filtered $ view (contents . to T.strip) >>> (== latestLabel)
    hrefToURI = attr "href" . tryParseURI


focusRelInfos :: Fold Node (Try (ReleaseInfo, URI))
focusRelInfos =
    ulListChapters . backwards (anchor . liftFold2 (liftA2 (,)) relInfo url)
  where
    ulListChapters = allNamed (only "ul") . attributed (hasClass "list-chapters")

    relInfo :: ToLike Element (Try ReleaseInfo)
    relInfo = to (view $ attr "title") . tryParseChapter mkChapterNo . to (fmap Episode)
      where
        mkChapterNo = parseEither $ string "Chapter " >> comicChapter

    url :: Fold Element (Try URI)
    url = attr "href" . tryParseURI


focusRelInfo :: Fold Node (Try ReleaseInfo)
focusRelInfo =
    ulChapList . elements . liCurrent . anchor . relInfo
  where
    ulChapList = allNamed (only "ul") . attributed (ix "id" . only "chap_list")
    liCurrent = named (only "li") . attributed (hasClass "current")
    relInfo = to (preview contents) . to (fmap T.strip) . tryParseChapter mkChapterNo . to (fmap Episode)
      where
        mkChapterNo = parseEither $ string "Chapter " >> comicChapter


focusImages :: Fold Node (Try URI)
focusImages =
    divChapterContent . imgChapterImg . url
  where
    divChapterContent = allNamed (only "div") . attributed (hasClass "chapter-content")
    imgChapterImg = allNamed (only "img") . attributed (hasClass "chapter-img")
    url = attr "src" . to (fmap (T.filter $ not . isSpace)) . tryParseURI
