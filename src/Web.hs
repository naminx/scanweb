{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Web where

import App.Exceptions
import Data.Char (isSpace)
import Data.Tuple.Extra (dupe)
import Import
import qualified RIO.Map as Map (fromList)
import qualified RIO.Text as T (dropWhileEnd, pack, strip)
import qualified RIO.Text.Lazy as TL (Text)
import Replace.Megaparsec (anyTill)
import Text.Megaparsec (eof)
import Text.Megaparsec.Char (string)
import Text.Taggy.Lens
import Text.URI (emptyURI, mkPathPiece, mkQueryValue, mkURI, render)
import Text.URI.Lens (queryParam, uriPath, uriQuery)
import Text.URI.QQ (queryKey, uri)

import qualified Web.HachiManga.Com as HachiManga.Com
import qualified Web.J8Jp.Com as J8Jp.Com
import qualified Web.KlManga.Net as KlManga.Net
import qualified Web.Manga1001.In as Manga1001.In
import qualified Web.MangaGun.Com as MangaGun.Com
import qualified Web.MangaRaw.Io as MangaRaw.Io
import qualified Web.MangaRaw.So as MangaRaw.So
import qualified Web.RawDevArt.Com as RawDevArt.Com
import qualified Web.WeLoMa.Art as WeLoMa.Art
import qualified Web.WeLoveManga.One as WeLoveManga.One


getNewReleaseUrl :: forall env s. (HasStateRef s env, HasApp s) => RIO env URI
getNewReleaseUrl = do
    web <- currentWeb <%= id
    webTab <- webTable <%= id
    domainName <- maybe (throwM $ LookupWebFailed web) return $ webTab ^? ix web . _1
    mangaList <- maybe (throwM $ LookupWebFailed web) return $ localMapping ^? ix web
    page <- currentPage <%= id
    https domainName <$> mangaList page
  where
    localMapping =
        Map.fromList
            [ (MangaRawSo, MangaRaw.So.newReleaseUrl)
            , (RawDevArtCom, RawDevArt.Com.newReleaseUrl)
            , (WeLoMaArt, WeLoMa.Art.newReleaseUrl)
            , (WeLoveMangaOne, WeLoveManga.One.newReleaseUrl)
            , (KlMangaNet, KlManga.Net.newReleaseUrl)
            , (HachiMangaCom, HachiManga.Com.newReleaseUrl)
            , (J8JpCom, J8Jp.Com.newReleaseUrl)
            , (MangaRawIo, MangaRaw.Io.newReleaseUrl)
            , (MangaGunCom, MangaGun.Com.newReleaseUrl)
            , (Manga1001In, Manga1001.In.newReleaseUrl)
            ]


keyElement :: forall env s. (HasStateRef s env, HasApp s) => RIO env Text
keyElement = do
    web <- currentWeb <%= id
    maybe (throwM $ LookupWebFailed web) return $ localMapping ^? ix web
  where
    localMapping =
        Map.fromList
            [ (MangaRawSo, MangaRaw.So.keyElement)
            , (RawDevArtCom, RawDevArt.Com.keyElement)
            , (WeLoMaArt, WeLoMa.Art.keyElement)
            , (WeLoveMangaOne, WeLoveManga.One.keyElement)
            , (KlMangaNet, KlManga.Net.keyElement)
            , (HachiMangaCom, HachiManga.Com.keyElement)
            , (J8JpCom, J8Jp.Com.keyElement)
            , (MangaRawIo, MangaRaw.Io.keyElement)
            , (MangaGunCom, MangaGun.Com.keyElement)
            , (Manga1001In, Manga1001.In.keyElement)
            ]


scrapeComics :: forall env s. (HasStateRef s env, HasApp s) => TL.Text -> RIO env [Try (URI, Maybe ReleaseInfo)]
scrapeComics markup = do
    web <- currentWeb <%= id
    webTab <- webTable <%= id
    domainName <- maybe (throwM $ LookupWebFailed web) return $ webTab ^? ix web . _1
    focusComics <- maybe (throwM $ LookupWebFailed web) return $ localMapping ^? ix web
    return $ markup ^.. html . focusComics . filtered (check domainName)
  where
    check domainName entry = entry ^? _Right . _1 . domain `elem` [Nothing, Just domainName]
    localMapping =
        Map.fromList
            [ (MangaRawSo, MangaRaw.So.focusComics)
            , (RawDevArtCom, RawDevArt.Com.focusComics)
            , (WeLoMaArt, WeLoMa.Art.focusComics)
            , (WeLoveMangaOne, WeLoveManga.One.focusComics)
            , (KlMangaNet, KlManga.Net.focusComics)
            , (HachiMangaCom, HachiManga.Com.focusComics)
            , (J8JpCom, J8Jp.Com.focusComics)
            , (MangaRawIo, MangaRaw.Io.focusComics)
            , (MangaGunCom, MangaGun.Com.focusComics)
            , (Manga1001In, Manga1001.In.focusComics)
            ]


scrapeLatestRelInfo :: forall env s. (HasStateRef s env, HasApp s) => TL.Text -> RIO env (Maybe (Try URI))
scrapeLatestRelInfo markup = do
    web <- currentWeb <%= id
    focusLatestRelInfo <- maybe (throwM $ LookupWebFailed web) return $ localMapping ^? ix web
    return $ markup ^? html . focusLatestRelInfo
  where
    localMapping =
        Map.fromList
            [ (MangaRawSo, MangaRaw.So.focusLatestRelInfo)
            , (RawDevArtCom, RawDevArt.Com.focusLatestRelInfo)
            , (WeLoMaArt, WeLoMa.Art.focusLatestRelInfo)
            , (WeLoveMangaOne, WeLoveManga.One.focusLatestRelInfo)
            , (KlMangaNet, KlManga.Net.focusLatestRelInfo)
            , (HachiMangaCom, HachiManga.Com.focusLatestRelInfo)
            , (J8JpCom, J8Jp.Com.focusLatestRelInfo)
            , (MangaRawIo, MangaRaw.Io.focusLatestRelInfo)
            , (MangaGunCom, MangaGun.Com.focusLatestRelInfo)
            , (Manga1001In, Manga1001.In.focusLatestRelInfo)
            ]


badChapters :: [URI]
badChapters =
    [ [uri|https://manga1000.top/sono-kisekae-ningyou-wa-koi-o-suru-raw-chap-86|]
    , [uri|https://manga1000.top/the-cafe-terrace-of-the-goddesses-raw-chap-108|]
    , [uri|https://syosetu.top/%e3%81%9d%e3%81%ae%e7%9d%80%e3%81%9b%e6%9b%bf%e3%81%88%e4%ba%ba%e5%bd%a2%e3%81%af%e6%81%8b%e3%82%92%e3%81%99%e3%82%8braw-free-chapter-86/|]
    , [uri|https://syosetu.top/%e3%82%b9%e3%83%88%e3%83%a9%e3%83%86%e3%82%b8%e3%83%83%e3%82%af%e3%83%a9%e3%83%90%e3%83%bc%e3%82%baraw-free-chapter-38/|]
    ]


scrapeRelInfos :: forall env s. (HasStateRef s env, HasApp s) => TL.Text -> RIO env [Try (ReleaseInfo, URI)]
scrapeRelInfos markup = do
    web <- currentWeb <%= id
    focusRelInfos <- maybe (throwM $ LookupWebFailed web) return $ localMapping ^? ix web
    return $ markup ^.. html . focusRelInfos . filteredBy (_Right . _2 . to (`notElem` badChapters))
  where
    localMapping =
        Map.fromList
            [ (MangaRawSo, MangaRaw.So.focusRelInfos)
            , (RawDevArtCom, RawDevArt.Com.focusRelInfos)
            , (WeLoMaArt, WeLoMa.Art.focusRelInfos)
            , (WeLoveMangaOne, WeLoveManga.One.focusRelInfos)
            , (KlMangaNet, KlManga.Net.focusRelInfos)
            , (HachiMangaCom, HachiManga.Com.focusRelInfos)
            , (J8JpCom, J8Jp.Com.focusRelInfos)
            , (MangaRawIo, MangaRaw.Io.focusRelInfos)
            , (MangaGunCom, MangaGun.Com.focusRelInfos)
            , (Manga1001In, Manga1001.In.focusRelInfos)
            ]


scrapeRelInfo :: forall env s. (HasStateRef s env, HasApp s) => TL.Text -> RIO env [Try ReleaseInfo]
scrapeRelInfo markup = do
    web <- currentWeb <%= id
    focusRelInfos <- maybe (throwM $ LookupWebFailed web) return $ localMapping ^? ix web
    return $ markup ^.. html . focusRelInfos
  where
    localMapping =
        Map.fromList
            [ (MangaRawSo, MangaRaw.So.focusRelInfo)
            , (RawDevArtCom, RawDevArt.Com.focusRelInfo)
            , (WeLoMaArt, WeLoMa.Art.focusRelInfo)
            , (WeLoveMangaOne, WeLoveManga.One.focusRelInfo)
            , (KlMangaNet, KlManga.Net.focusRelInfo)
            , (HachiMangaCom, HachiManga.Com.focusRelInfo)
            , (J8JpCom, J8Jp.Com.focusRelInfo)
            , -- (MangaRawIo, MangaRaw.Io.focusRelInfo)
              (MangaGunCom, MangaGun.Com.focusRelInfo)
            , (Manga1001In, Manga1001.In.focusRelInfo)
            ]


scrapeImages :: forall env s. (HasStateRef s env, HasApp s) => TL.Text -> RIO env [Try URI]
scrapeImages markup = do
    web <- currentWeb <%= id
    focusImages <- maybe (throwM $ LookupWebFailed web) return $ localMapping ^? ix web
    return $ markup ^.. html . focusImages
  where
    localMapping =
        Map.fromList
            [ (MangaRawSo, MangaRaw.So.focusImages)
            , (RawDevArtCom, RawDevArt.Com.focusImages)
            , (WeLoMaArt, WeLoMa.Art.focusImages)
            , (WeLoveMangaOne, WeLoveManga.One.focusImages)
            , (KlMangaNet, KlManga.Net.focusImages)
            , (HachiMangaCom, HachiManga.Com.focusImages)
            , (J8JpCom, J8Jp.Com.focusImages)
            , (MangaRawIo, MangaRaw.Io.focusImages)
            , (MangaGunCom, MangaGun.Com.focusImages)
            , (Manga1001In, Manga1001.In.focusImages)
            ]
