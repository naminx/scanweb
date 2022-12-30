{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Web.Common.MangaHatachi where

import Import
import qualified RIO.Text as T
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


webIdentity :: Text
webIdentity = "a.logo"


focusComics :: Fold Node (Try (URI, Maybe ReleaseInfo))
focusComics =
    divMainColInnerCPage
        . divItemSummary
        . liftFold2 (liftA2 (,)) comics relInfo
  where
    divMainColInnerCPage =
        allNamed (only "div")
            . attributed (hasClasses ["main-col-inner", "c-page"])
            . notAttributed (hasClass "main-sticky-mangas")

    divItemSummary =
        allNamed (only "div")
            . attributed (hasClass "item-summary")

    comics :: Fold Element (Try URI)
    comics = h3H5 . anchor . attr "href" . tryParseURI

    h3H5 :: Fold Element Element
    h3H5 = allNamed (only "h3") . attributed (hasClass "h5")

    relInfo :: ToLike Element (Try (Maybe ReleaseInfo))
    relInfo =
        to (preview $ taking 1 divChapterItem . anchor . contents)
            . tryParseRelInfo (parseEither comicRelInfo)
            . to (fmap Just)

    comicRelInfo = do
        (_, r) <- anyTill comicReleaseInfo
        _ <- anyTill eof
        return r

    divChapterItem =
        allNamed (only "div")
            . attributed (hasClass "chapter-item")


focusLatestRelInfo :: Fold Node (Try URI)
focusLatestRelInfo = noLatestRelInfo


focusRelInfos :: Fold Node (Try (ReleaseInfo, URI))
focusRelInfos =
    backwards $
        liWpMangaChapter
            . anchorNotCNewTag
            . liftFold2 (liftA2 (,)) relInfo url
  where
    liWpMangaChapter =
        allNamed (only "li")
            . attributed (hasClass "wp-manga-chapter")
    anchorNotCNewTag =
        allNamed (only "a")
            . notAttributed (hasClass "c-new-tag")

    relInfo :: ToLike Element (Try ReleaseInfo)
    relInfo =
        to (preview $ contents . to T.strip) . tryParseRelInfo mkReleaseInfo

    url :: Fold Element (Try URI)
    url = attr "href" . tryParseURI


focusRelInfo :: Fold Node (Try ReleaseInfo)
focusRelInfo =
    taking 1 selectSingleChapter . optionSelected . relInfo
  where
    selectSingleChapter =
        allNamed (only "select")
            . attributed (hasClass "single-chapter-select")
    optionSelected =
        elements
            . allNamed (only "option")
            . attributed (ix "selected" . only "selected")

    relInfo = attr "value" . tryParseChapter mkChapterNo . to (fmap Episode)
      where
        mkChapterNo = parseEither $ string "Chapter " >> comicChapter


focusImages :: Fold Node (Try URI)
focusImages =
    imgWpMangaChapterImg . attr "data-src" . to (fmap T.strip) . tryParseURI
  where
    imgWpMangaChapterImg =
        allNamed (only "img")
            . attributed (hasClass "wp-manga-chapter-img")
