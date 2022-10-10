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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Web where

import App.Chapter
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

import qualified Web.J9Jp.Com as J9Jp.Com
import qualified Web.KlManga.Net as KlManga.Net
import qualified Web.Manga9.Co as Manga9.Co
import qualified Web.MangaGun.Com as MangaGun.Com
import qualified Web.MangaHatachi.Com as MangaHatachi.Com
import qualified Web.MangaRaw.Io as MangaRaw.Io
import qualified Web.RawDevArt.Com as RawDevArt.Com
import qualified Web.SyoSetu.Top as SyoSetu.Top
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
                [ (MangaRawIo, MangaRaw.Io.newReleaseUrl)
                , (RawDevArtCom, RawDevArt.Com.newReleaseUrl)
                , (WeLoMaArt, WeLoMa.Art.newReleaseUrl)
                , (WeLoveMangaOne, WeLoveManga.One.newReleaseUrl)
                , (KlMangaNet, KlManga.Net.newReleaseUrl)
                , (MangaGunCom, MangaGun.Com.newReleaseUrl)
                , (J9JpCom, J9Jp.Com.newReleaseUrl)
                , (Manga9Co, Manga9.Co.newReleaseUrl)
                , (SyoSetuTop, SyoSetu.Top.newReleaseUrl)
                , (MangaHatachiCom, MangaHatachi.Com.newReleaseUrl)
                ]


keyElement :: forall env s. (HasStateRef s env, HasApp s) => RIO env Text
keyElement = do
    web <- currentWeb <%= id
    maybe (throwM $ LookupWebFailed web) return $ localMapping ^? ix web
    where
        localMapping =
            Map.fromList
                [ (MangaRawIo, MangaRaw.Io.keyElement)
                , (RawDevArtCom, RawDevArt.Com.keyElement)
                , (WeLoMaArt, WeLoMa.Art.keyElement)
                , (WeLoveMangaOne, WeLoveManga.One.keyElement)
                , (KlMangaNet, KlManga.Net.keyElement)
                , (MangaGunCom, MangaGun.Com.keyElement)
                , (J9JpCom, J9Jp.Com.keyElement)
                , (Manga9Co, Manga9.Co.keyElement)
                , (SyoSetuTop, SyoSetu.Top.keyElement)
                , (MangaHatachiCom, MangaHatachi.Com.keyElement)
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
                [ (MangaRawIo, MangaRaw.Io.focusComics)
                , (RawDevArtCom, RawDevArt.Com.focusComics)
                , (WeLoMaArt, WeLoMa.Art.focusComics)
                , (WeLoveMangaOne, WeLoveManga.One.focusComics)
                , (KlMangaNet, KlManga.Net.focusComics)
                , (MangaGunCom, MangaGun.Com.focusComics)
                , (J9JpCom, J9Jp.Com.focusComics)
                , (Manga9Co, Manga9.Co.focusComics)
                , (SyoSetuTop, SyoSetu.Top.focusComics)
                , (MangaHatachiCom, MangaHatachi.Com.focusComics)
                ]


scrapeLatestRelInfo :: forall env s. (HasStateRef s env, HasApp s) => TL.Text -> RIO env (Maybe (Try URI))
scrapeLatestRelInfo markup = do
    web <- currentWeb <%= id
    focusLatestRelInfo <- maybe (throwM $ LookupWebFailed web) return $ localMapping ^? ix web
    return $ markup ^? html . focusLatestRelInfo
    where
        localMapping =
            Map.fromList
                [ (MangaRawIo, MangaRaw.Io.focusLatestRelInfo)
                , (RawDevArtCom, RawDevArt.Com.focusLatestRelInfo)
                , (WeLoMaArt, WeLoMa.Art.focusLatestRelInfo)
                , (WeLoveMangaOne, WeLoveManga.One.focusLatestRelInfo)
                , (KlMangaNet, KlManga.Net.focusLatestRelInfo)
                , (MangaGunCom, MangaGun.Com.focusLatestRelInfo)
                , (J9JpCom, J9Jp.Com.focusLatestRelInfo)
                , (Manga9Co, Manga9.Co.focusLatestRelInfo)
                , (SyoSetuTop, SyoSetu.Top.focusLatestRelInfo)
                , (MangaHatachiCom, MangaHatachi.Com.focusLatestRelInfo)
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
                [ (MangaRawIo, MangaRaw.Io.focusRelInfos)
                , (RawDevArtCom, RawDevArt.Com.focusRelInfos)
                , (WeLoMaArt, WeLoMa.Art.focusRelInfos)
                , (WeLoveMangaOne, WeLoveManga.One.focusRelInfos)
                , (KlMangaNet, KlManga.Net.focusRelInfos)
                , (MangaGunCom, MangaGun.Com.focusRelInfos)
                , (J9JpCom, J9Jp.Com.focusRelInfos)
                , (Manga9Co, Manga9.Co.focusRelInfos)
                , (SyoSetuTop, SyoSetu.Top.focusRelInfos)
                , (MangaHatachiCom, MangaHatachi.Com.focusRelInfos)
                ]


scrapeRelInfo :: forall env s. (HasStateRef s env, HasApp s) => TL.Text -> RIO env [Try ReleaseInfo]
scrapeRelInfo markup = do
    web <- currentWeb <%= id
    focusRelInfos <- maybe (throwM $ LookupWebFailed web) return $ localMapping ^? ix web
    return $ markup ^.. html . focusRelInfos
    where
        localMapping =
            Map.fromList
                -- [ (MangaRawIo, MangaRaw.Io.focusRelInfo)
                [ (RawDevArtCom, RawDevArt.Com.focusRelInfo)
                , (WeLoMaArt, WeLoMa.Art.focusRelInfo)
                , (WeLoveMangaOne, WeLoveManga.One.focusRelInfo)
                , (KlMangaNet, KlManga.Net.focusRelInfo)
                , (MangaGunCom, MangaGun.Com.focusRelInfo)
                , (J9JpCom, J9Jp.Com.focusRelInfo)
                , (Manga9Co, Manga9.Co.focusRelInfo)
                , (SyoSetuTop, SyoSetu.Top.focusRelInfo)
                , (MangaHatachiCom, MangaHatachi.Com.focusRelInfo)
                ]


scrapeImages :: forall env s. (HasStateRef s env, HasApp s) => TL.Text -> RIO env [Try URI]
scrapeImages markup = do
    web <- currentWeb <%= id
    focusImages <- maybe (throwM $ LookupWebFailed web) return $ localMapping ^? ix web
    return $ markup ^.. html . focusImages
    where
        localMapping =
            Map.fromList
                [ (MangaRawIo, MangaRaw.Io.focusImages)
                , (RawDevArtCom, RawDevArt.Com.focusImages)
                , (WeLoMaArt, WeLoMa.Art.focusImages)
                , (WeLoveMangaOne, WeLoveManga.One.focusImages)
                , (KlMangaNet, KlManga.Net.focusImages)
                , (MangaGunCom, MangaGun.Com.focusImages)
                , (J9JpCom, J9Jp.Com.focusImages)
                , (Manga9Co, Manga9.Co.focusImages)
                , (SyoSetuTop, SyoSetu.Top.focusImages)
                , (MangaHatachiCom, MangaHatachi.Com.focusImages)
                ]
