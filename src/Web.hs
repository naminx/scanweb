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
import qualified Web.Manga9.Co as Manga9.Co
import qualified Web.KlManga.Net as KlManga.Net
import qualified Web.Manga1001.Top as Manga1001.Top
import qualified Web.MangaRaw.Co as MangaRaw.Co
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
            [ (MangaRaw, MangaRaw.Co.newReleaseUrl)
            , (RawDevArt, RawDevArt.Com.newReleaseUrl)
            , (WeLoMa, WeLoMa.Art.newReleaseUrl)
            , (WeLoveManga, WeLoveManga.One.newReleaseUrl)
            , (KlManga, KlManga.Net.newReleaseUrl)
            , (J9Jp, J9Jp.Com.newReleaseUrl)
            , (Manga9, Manga9.Co.newReleaseUrl)
            , (SyoSetu, SyoSetu.Top.newReleaseUrl)
            , (Manga1001, Manga1001.Top.newReleaseUrl)
            ]


keyElement :: forall env s. (HasStateRef s env, HasApp s) => RIO env Text
keyElement = do
    web <- currentWeb <%= id
    maybe (throwM $ LookupWebFailed web) return $ localMapping ^? ix web
  where
    localMapping =
        Map.fromList
            [ (MangaRaw, MangaRaw.Co.keyElement)
            , (RawDevArt, RawDevArt.Com.keyElement)
            , (WeLoMa, WeLoMa.Art.keyElement)
            , (WeLoveManga, WeLoveManga.One.keyElement)
            , (KlManga, KlManga.Net.keyElement)
            , (J9Jp, J9Jp.Com.keyElement)
            , (Manga9, Manga9.Co.keyElement)
            , (SyoSetu, SyoSetu.Top.keyElement)
            , (Manga1001, Manga1001.Top.keyElement)
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
            [ (MangaRaw, MangaRaw.Co.focusComics)
            , (RawDevArt, RawDevArt.Com.focusComics)
            , (WeLoMa, WeLoMa.Art.focusComics)
            , (WeLoveManga, WeLoveManga.One.focusComics)
            , (KlManga, KlManga.Net.focusComics)
            , (J9Jp, J9Jp.Com.focusComics)
            , (Manga9, Manga9.Co.focusComics)
            , (SyoSetu, SyoSetu.Top.focusComics)
            , (Manga1001, Manga1001.Top.focusComics)
            ]


scrapeLatestRelInfo :: forall env s. (HasStateRef s env, HasApp s) => TL.Text -> RIO env (Maybe (Try URI))
scrapeLatestRelInfo markup = do
    web <- currentWeb <%= id
    focusLatestRelInfo <- maybe (throwM $ LookupWebFailed web) return $ localMapping ^? ix web
    return $ markup ^? html . focusLatestRelInfo
  where
    localMapping =
        Map.fromList
            [ (MangaRaw, MangaRaw.Co.focusLatestRelInfo)
            , (RawDevArt, RawDevArt.Com.focusLatestRelInfo)
            , (WeLoMa, WeLoMa.Art.focusLatestRelInfo)
            , (WeLoveManga, WeLoveManga.One.focusLatestRelInfo)
            , (KlManga, KlManga.Net.focusLatestRelInfo)
            , (J9Jp, J9Jp.Com.focusLatestRelInfo)
            , (Manga9, Manga9.Co.focusLatestRelInfo)
            , (SyoSetu, SyoSetu.Top.focusLatestRelInfo)
            , (Manga1001, Manga1001.Top.focusLatestRelInfo)
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
            [ (MangaRaw, MangaRaw.Co.focusRelInfos)
            , (RawDevArt, RawDevArt.Com.focusRelInfos)
            , (WeLoMa, WeLoMa.Art.focusRelInfos)
            , (WeLoveManga, WeLoveManga.One.focusRelInfos)
            , (KlManga, KlManga.Net.focusRelInfos)
            , (J9Jp, J9Jp.Com.focusRelInfos)
            , (Manga9, Manga9.Co.focusRelInfos)
            , (SyoSetu, SyoSetu.Top.focusRelInfos)
            , (Manga1001, Manga1001.Top.focusRelInfos)
            ]


scrapeRelInfo :: forall env s. (HasStateRef s env, HasApp s) => TL.Text -> RIO env [Try ReleaseInfo]
scrapeRelInfo markup = do
    web <- currentWeb <%= id
    focusRelInfos <- maybe (throwM $ LookupWebFailed web) return $ localMapping ^? ix web
    return $ markup ^.. html . focusRelInfos
  where
    localMapping =
        Map.fromList
            -- [ (MangaRaw, MangaRaw.Co.focusRelInfo)
            [ (RawDevArt, RawDevArt.Com.focusRelInfo)
            , (WeLoMa, WeLoMa.Art.focusRelInfo)
            , (WeLoveManga, WeLoveManga.One.focusRelInfo)
            , (KlManga, KlManga.Net.focusRelInfo)
            , (J9Jp, J9Jp.Com.focusRelInfo)
            , (Manga9, Manga9.Co.focusRelInfo)
            , (SyoSetu, SyoSetu.Top.focusRelInfo)
            -- , (Manga1001, Manga1001.Top.focusRelInfo)
            ]


scrapeImages :: forall env s. (HasStateRef s env, HasApp s) => TL.Text -> RIO env [Try URI]
scrapeImages markup = do
    web <- currentWeb <%= id
    focusImages <- maybe (throwM $ LookupWebFailed web) return $ localMapping ^? ix web
    return $ markup ^.. html . focusImages
  where
    localMapping =
        Map.fromList
            [ (MangaRaw, MangaRaw.Co.focusImages)
            , (RawDevArt, RawDevArt.Com.focusImages)
            , (WeLoMa, WeLoMa.Art.focusImages)
            , (WeLoveManga, WeLoveManga.One.focusImages)
            , (KlManga, KlManga.Net.focusImages)
            , (J9Jp, J9Jp.Com.focusImages)
            , (Manga9, Manga9.Co.focusImages)
            , (SyoSetu, SyoSetu.Top.focusImages)
            , (Manga1001, Manga1001.Top.focusImages)
            ]
