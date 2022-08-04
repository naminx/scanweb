{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Web.Common.MangaHatachi where

import App.Chapter
import App.Exceptions
import Import
import qualified RIO.Text as T
import qualified Text.Megaparsec as MP (try)
import Text.Megaparsec.Char (string)
import Text.Megaparsec.Char.Lexer (decimal)
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
        return $ [uri|/page/|] & uriPath %~ (<> pageNo)
  where
    pageNo = mkPathPiece $ T.pack $ show n


webIdentity :: Text
webIdentity = "a.logo"


focusComics :: Fold Node (Try (URI, Maybe ReleaseInfo))
focusComics = divMainColInnerCPage . divItemSummary . liftFold2 (liftA2 (,)) comics relInfo
  where
    divMainColInnerCPage =
        allNamed (only "div") . attributed (hasClasses ["main-col-inner", "c-page"])
            . notAttributed (hasClass "main-sticky-mangas")

    divItemSummary = allNamed (only "div") . attributed (hasClass "item-summary")

    comics :: Fold Element (Try URI)
    comics = h3H5 . anchor . attr "href" . tryParseURI

    h3H5 :: Fold Element Element
    h3H5 = allNamed (only "h3") . attributed (hasClass "h5")

    relInfo :: ToLike Element (Try (Maybe ReleaseInfo))
    relInfo =
        to (preview $ taking 1 divChapterItem . anchor . contents)
            . tryParseRelInfo
            . to (fmap Just)

    divChapterItem = allNamed (only "div") . attributed (hasClass "chapter-item")

    tryParseRelInfo :: ToLike (Maybe Text) (Try ReleaseInfo)
    tryParseRelInfo = to (maybeToTry ChapterNoNotFound) . to (>>= T.strip >>> mkReleaseInfo)


focusLatestRelInfo :: Fold Node (Try URI)
focusLatestRelInfo = noLatestRelInfo


focusRelInfos :: Fold Node (Try (ReleaseInfo, URI))
focusRelInfos =
    backwards $ liWpMangaChapter . anchorNotCNewTag . liftFold2 (liftA2 (,)) relInfo url
  where
    liWpMangaChapter = allNamed (only "li") . attributed (hasClass "wp-manga-chapter")
    anchorNotCNewTag = allNamed (only "a") . notAttributed (hasClass "c-new-tag")

    relInfo :: ToLike Element (Try ReleaseInfo)
    relInfo = to (preview $ contents . to T.strip) . tryParseRelInfo

    url :: Fold Element (Try URI)
    url = attr "href" . tryParseURI

    tryParseRelInfo :: ToLike (Maybe Text) (Try ReleaseInfo)
    tryParseRelInfo = to (maybeToTry ChapterNoNotFound) . to (>>= mkReleaseInfo)


mkReleaseInfo :: Text -> Try ReleaseInfo
mkReleaseInfo = parseEither relInfo
  where
    relInfo = MP.try episode <|> MP.try book <|> episodes

    episode = string "第" >> comicChapter >>= (string "話" >>) . return . Episode

    book = string "第" >> decimal >>= (string "巻" >>) . return . Book . Volume

    episodes = do
        _ <- string "第"
        beginChap <- comicChapter
        _ <- string "-"
        endChap <- comicChapter
        _ <- string "話"
        return $ Episodes (beginChap, endChap)


focusImages :: Fold Node (Try URI)
focusImages =
    imgWpMangaChapterImg . attr "data-src" . to (fmap T.strip) . tryParseURI
  where
    imgWpMangaChapterImg = allNamed (only "img") . attributed (hasClass "wp-manga-chapter-img")
