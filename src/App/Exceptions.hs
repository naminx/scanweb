{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module App.Exceptions where

import Control.Monad.Script.Http (E (..))
import Import hiding (domain)
import Web.Api.WebDriver (WDError)


newtype InvalidWebNo = InvalidWebNo Int
    deriving (Eq, Show)


instance Exception InvalidWebNo where
    displayException (InvalidWebNo webNo) =
        "Invalid web number: " <> show webNo


newtype LookupWebFailed = LookupWebFailed Web
    deriving (Eq, Show)


instance Exception LookupWebFailed where
    displayException (LookupWebFailed web) =
        "Web \"" <> show web <> "\" not found in database"


newtype LookupWebComicFailed = LookupWebComicFailed (Web, Comic)
    deriving (Eq, Show)


instance Exception LookupWebComicFailed where
    displayException (LookupWebComicFailed (web, Comic comic)) =
        show web <> "\" does not hosts comic No." <> show comic


newtype LookupComicFailed = LookupComicFailed Comic
    deriving (Eq, Show)


instance Exception LookupComicFailed where
    displayException (LookupComicFailed comic) =
        "Comic No." <> show (unComic comic) <> " not found in database"


data ComicLinksNotFound = ComicLinksNotFound
    deriving (Eq, Show)


instance Exception ComicLinksNotFound where
    displayException ComicLinksNotFound =
        "Parse failed, comic links not found"


data ChapterLinksNotFound = ChapterLinksNotFound
    deriving (Eq, Show)


instance Exception ChapterLinksNotFound where
    displayException ChapterLinksNotFound =
        "Parse failed, chapter links not found"


data ChapterNoNotFound = ChapterNoNotFound
    deriving (Eq, Show)


instance Exception ChapterNoNotFound where
    displayException ChapterNoNotFound =
        "Parse failed, chapter No. not found"


data ImageLinkNotFound = ImageLinkNotFound
    deriving (Eq, Show)


instance Exception ImageLinkNotFound where
    displayException ImageLinkNotFound =
        "Parse failed, image link not found"


newtype InvalidContentType = InvalidContentType String
    deriving (Eq, Show)


instance Exception InvalidContentType where
    displayException (InvalidContentType contentType) =
        "Invalid content type \"" <> contentType <> "\""


newtype InvalidImageSubtype = InvalidImageSubtype String
    deriving (Eq, Show)


instance Exception InvalidImageSubtype where
    displayException (InvalidImageSubtype imageSubtype) =
        "Invalid image subtype \"" <> imageSubtype <> "\""


data SomeImagesNotDownloaded = SomeImagesNotDownloaded
    deriving (Eq, Show)


instance Exception SomeImagesNotDownloaded where
    displayException SomeImagesNotDownloaded =
        "Some images not downloaded"


data SomeChaptersNotDownloaded = SomeChaptersNotDownloaded
    deriving (Eq, Show)


instance Exception SomeChaptersNotDownloaded where
    displayException SomeChaptersNotDownloaded =
        "Some chapters not downloaded"


newtype UnknownDomainName = UnknownDomainName String
    deriving (Eq, Show)


instance Exception UnknownDomainName where
    displayException (UnknownDomainName domain) =
        "Unknown domain name: " <> domain


newtype InvalidBaseUrl = InvalidBaseUrl String
    deriving (Eq, Show)


instance Exception InvalidBaseUrl where
    displayException (InvalidBaseUrl url) =
        "Invalid base URL: " <> url


newtype UnexpectedQueryResult = UnexpectedQueryResult String
    deriving (Eq, Show)


instance Exception UnexpectedQueryResult where
    displayException (UnexpectedQueryResult result) =
        "Unexpected query result: " <> result


newtype UpdateDatabaseFailed = UpdateDatabaseFailed String
    deriving (Eq, Show)


instance Exception UpdateDatabaseFailed where
    displayException (UpdateDatabaseFailed query) =
        "Update database failed: " <> query


instance Exception WDError where
    displayException wdError = show wdError


instance Exception e => Exception (E e) where
    displayException e = show e
