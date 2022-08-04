{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module App.Types where

import App.Chapter (Chapter, emptyChapter)
import Control.Lens (each, makeClassy, makeFieldsNoPrefix, to, (.~), (^.), (^..))
import Data.Aeson (FromJSON)
import Data.WOE (IsoEnum (mapping), WOE (WOE))
import Database.Esqueleto.Experimental (SqlBackend)
import Database.Persist.TH (mkPersist, persistLowerCase, share, sqlSettings)
import Lib
import Network.Wreq.Session (Session)
import Path (Abs, Dir, File, Path, Rel, SomeBase, reldir)
import RIO.Process (HasProcessContext (processContextL), ProcessContext)
import qualified RIO.Text as T (intercalate, unpack)
import Text.URI (URI, UserInfo, emptyURI, render, unRText)
import Text.URI.Lens hiding (unRText)
import Text.URI.QQ (host)
import Web.Api.WebDriver (ContextId, SessionId)


newtype URL = URL {getURI :: URI}
    deriving (Eq)


instance Show URL where
    show (URL url) = T.unpack $ renderUrl url
      where
        renderUrl u =
            "[uri|"
                <> render (emptyURI & uriScheme .~ u ^. uriScheme & uriAuthority .~ u ^. uriAuthority)
                <> u ^. uriAuthority . to (isRight >>> trueToSlash)
                <> (u ^.. uriPath . each . to unRText & T.intercalate "/")
                <> u ^. uriTrailingSlash . to trueToSlash
                <> render (emptyURI & uriQuery .~ u ^. uriQuery & uriFragment .~ u ^. uriFragment)
                <> "|]"

        trueToSlash = bool "" "/"


data Web
    = MangaRaw
    | RawDevArt
    | WeLoMa
    | WeLoveManga
    | KlManga
    | J9Jp
    | Manga9
    | SyoSetu
    | Manga1001
    deriving (Eq, Ord, Bounded, Show, Generic)
    deriving (Enum) via WOE Web


instance Hashable Web


-- make `Page` parametric type in order to derive `Functor`
newtype Page a = Page a
    deriving (Eq, Functor, Ord, Show)


instance IsoEnum Web where
    mapping =
        [ (0, MangaRaw)
        , (1, RawDevArt)
        , (2, WeLoMa)
        , (3, WeLoveManga)
        , (4, KlManga)
        , (5, J9Jp)
        , (6, Manga9)
        , (7, SyoSetu)
        , (8, Manga1001)
        ]


type WebInfo = (Domain, Maybe UserInfo, URI)
type WebTable = Map Web WebInfo
type DomainTable = Map Domain Web
newtype Comic = Comic {unComic :: Int} deriving (Eq, Ord, Show)
newtype Volume = Volume {unVolume :: Int} deriving (Eq, Ord)
newtype Title = Title {unTitle :: Text} deriving (Eq, Ord, Show)
type ComicInfo = (Title, Path Rel Dir, Volume, Chapter)
type ComicTable = Map Comic ComicInfo
type UrlTable = Map URI (Web, Comic)


instance Show Volume where
    show (Volume volume) = show volume


data ReleaseInfo
    = Episode Chapter
    | Episodes (Chapter, Chapter)
    | Book Volume
    deriving (Eq, Show)


instance Ord ReleaseInfo where
    Episode a < Episode b = a < b
    Episode a < Episodes (b, _) = a < b
    Episodes _ < Book _ = False
    Episodes (_, a) < Episode b = a < b
    Episodes (_, a) < Episodes (b, _) = a < b
    Episode _ < Book _ = False
    Book _ < Episode _ = True
    Book _ < Episodes _ = True
    Book a < Book b = a < b


    Episode a > Episode b = a > b
    Episode a > Episodes (_, b) = a > b
    Episodes _ > Book _ = False
    Episodes (a, _) > Episode b = a > b
    Episodes (a, _) > Episodes (_, b) = a > b
    Episode _ > Book _ = False
    Book _ > Episode _ = True
    Book _ > Episodes _ = True
    Book a > Book b = a > b


    compare a b =
        if a == b
            then EQ
            else
                if a < b && b > a
                    then LT
                    else
                        if a > b && b < a
                            then GT
                            else error $ "comparing overlapping range: " <> show a <> " vs " <> show b


emptyWebInfo :: WebInfo
emptyWebInfo = ([host||], Nothing, emptyURI)


emptyComicInfo :: ComicInfo
emptyComicInfo = (Title "", [reldir|.|], Volume 0, emptyChapter)


data AppMode
    = ScanWebs [Web]
    | UpdateComic (Web, Comic, Maybe ReleaseInfo)
    | DownloadRelease (Web, Comic, ReleaseInfo)
    | ListWebs
    | ListComics
    deriving (Eq, Show)


-- | Command line arguments
data Options = Options
    { _rootDir :: !(Path Abs Dir)
    , _dbFile :: !(SomeBase File)
    , _appMode :: !AppMode
    , _maxNumPages :: !Int
    }
    deriving (Eq, Show)


makeFieldsNoPrefix ''Options


data App = App
    { _logFunc :: !LogFunc
    , _processContext :: !ProcessContext
    , _options :: !Options
    , -- Add other app-specific configuration information here
      _currentWeb :: !Web
    , _currentPage :: !(Page Int)
    , _currentSqlBackend :: !SqlBackend
    , _currentWrqSession :: !Session
    , _webTable :: WebTable
    , _domainTable :: DomainTable
    , _comicTable :: ComicTable
    , _urlTable :: UrlTable
    , _currentWdSession :: SessionId
    , _currentWebInfo :: !WebInfo
    , _currentComic :: !Comic
    , _currentComicUrl :: !URI
    , _currentComicInfo :: !ComicInfo
    , _currentComicWindow :: !ContextId
    , _currentReferer :: !URI
    , _newReleaseInfo :: !(Maybe ReleaseInfo)
    , _stickyLine :: !Text
    }


makeClassy ''App


instance HasLogFunc App where
    logFuncL = logFunc


instance HasProcessContext App where
    processContextL = processContext


data Link = Link
    { _href :: Text
    , _caption :: Text
    }
    deriving (Eq, FromJSON, Generic, Ord, Show)


makeFieldsNoPrefix ''Link


share
    [mkPersist sqlSettings]
    [persistLowerCase|
  Webs
    web Int
    domain Text
    username Text Maybe
    password Text Maybe
    sentinel Text
    Primary web
    deriving Eq Show
  Comics
    comic Int Primary
    title Text
    folder Text
    volume Int
    chapter Int
    section Int Maybe
    Primary comic
    deriving Eq Show
  Urls
    web Int
    comic Int
    path Text
    Primary web comic
    deriving Eq Show
|]
