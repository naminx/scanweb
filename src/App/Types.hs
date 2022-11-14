{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
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

import App.Chapter (Chapter, emptyChapter, mkChapter)
import Control.Lens (each, makeClassy, makeFieldsNoPrefix, to, (.~), (^.), (^..))
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..))
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Scientific (toBoundedInteger)
import Data.WOE (IsoEnum (..), WOE (..), fromEnumSafely, toEnumSafely)
import Database.Esqueleto.Experimental (
    PersistField (..),
    PersistFieldSql (..),
    PersistValue (..),
    SqlBackend,
    SqlType (..),
 )
import qualified Database.Esqueleto.Internal.Internal as ES (SqlString)
import Database.Persist.TH (mkPersist, persistLowerCase, share, sqlSettings)
import Lib
import Path (Abs, Dir, File, Path, Rel, SomeBase, parseRelDir, reldir, toFilePath)
import RIO.Process (HasProcessContext (processContextL), ProcessContext)
import qualified RIO.Text as T (intercalate, pack, unpack)
import Text.URI (
    RText,
    RTextLabel (..),
    URI,
    UserInfo,
    emptyURI,
    mkHost,
    mkPassword,
    mkURI,
    mkUsername,
    render,
    unRText,
 )
import Text.URI.Lens hiding (unRText)
import Text.URI.QQ (host)
import Web.Api.WebDriver (ContextId, SessionId)
import Web.Internal.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))
import Web.PathPieces (PathPiece (..), readFromPathPiece, showToPathPiece)


newtype URL = URL {getURI :: URI}
    deriving (Eq)


instance Show URL where
    show (URL url) = T.unpack $ renderUrl url
      where
        renderUrl u =
            "[uri|"
                <> render (u & uriPath .~ [] & uriQuery .~ [] & uriFragment .~ Nothing)
                <> (u ^. uriAuthority . to (isRight >>> trueToSlash))
                <> (u ^.. uriPath . each . to unRText & T.intercalate "/")
                <> (u ^. uriTrailingSlash . to trueToSlash)
                <> render (u & uriScheme .~ Nothing & uriAuthority .~ Left False & uriPath .~ [])
                <> "|]"

        trueToSlash = bool "" "/"


data Web
    = MangaRawIo
    | RawDevArtCom
    | WeLoMaArt
    | WeLoveMangaOne
    | KlMangaNet
    | MangaHatachiCom
    | J9JpCom
    | SyoSetuTop
    | MangaGunCom
    | Manga9Co
    deriving (Eq, Ord, Bounded, Show, Generic, Read)
    deriving (Enum) via WOE Web


instance IsoEnum Web where
    mapping =
        [ (0, MangaRawIo)
        , (1, RawDevArtCom)
        , (2, WeLoMaArt)
        , (3, WeLoveMangaOne)
        , (4, KlMangaNet)
        , (5, MangaHatachiCom)
        , (6, J9JpCom)
        , (7, SyoSetuTop)
        , (8, MangaGunCom)
        , (9, Manga9Co)
        ]


instance Hashable Web


instance ToJSON Web where
    toJSON = Number . fromIntegral . fromMaybe raiseError . fromEnumSafely
      where
        raiseError = error "internal error: missing value in IsoEnum mapping for Web type"


instance FromJSON Web where
    parseJSON (Number n) =
        maybe
            (fail $ "parsing Web failed, value out of bound: " ++ show n)
            pure
            (toBoundedInteger n >>= toEnumSafely)
    parseJSON invalid =
        prependFailure
            "parsing Web failed, "
            (typeMismatch "Number" invalid)


instance PathPiece Web where
    toPathPiece = showToPathPiece . fromMaybe raiseError . fromEnumSafely
      where
        raiseError = error "internal error: missing value in IsoEnum mapping for Web type"
    fromPathPiece = readFromPathPiece >=> toEnumSafely


instance ToHttpApiData Web where
    toUrlPiece = toPathPiece


instance FromHttpApiData Web where
    parseUrlPiece x = parseUrlPiece x >>= maybe leftValue Right . toEnumSafely
      where
        leftValue = Left "Web value out of bound"


instance PersistField Web where
    toPersistValue = PersistInt64 . fromIntegral . fromMaybe raiseError . fromEnumSafely
      where
        raiseError = error "internal error: missing value in IsoEnum mapping for Web type"
    fromPersistValue (PersistInt64 n) = maybe toErrorMsg Right $ toEnumSafely $ fromIntegral n
      where
        toErrorMsg = Left $ "converting to Web failed, value out of bound: " <> T.pack (show n)
    fromPersistValue invalid =
        Left $ "reading Web failed, expected PersistInt64, received: " <> T.pack (show invalid)


instance PersistFieldSql Web where
    sqlType _ = SqlInt64


-- make `Page` parametric type in order to derive `Functor`
newtype Page a = Page a
    deriving (Eq, Functor, Ord, Show)


type WebInfo = (Domain, Maybe UserInfo, URI)
type WebTable = Map Web WebInfo
type DomainTable = Map Domain Web
newtype Comic = Comic {unComic :: Int}
    deriving (Eq, Ord, Show, Generic, Read)
    deriving newtype (FromJSON, ToJSON, PathPiece, ToHttpApiData, FromHttpApiData)
newtype Volume = Volume {unVolume :: Int} deriving (Eq, Ord)
newtype Title = Title {unTitle :: Text} deriving (Eq, Ord, Show)
type ComicInfo = (Title, Path Rel Dir, Volume, Chapter)
type ComicTable = Map Comic ComicInfo
type UrlTable = Map URI (Web, Comic)


instance PersistField Comic where
    toPersistValue (Comic n) = PersistInt64 $ fromIntegral n
    fromPersistValue (PersistInt64 n) = Right $ Comic $ fromIntegral n
    fromPersistValue invalid =
        Left $ "reading `Comic` failed, expected `PersistInt64`, received: " <> T.pack (show invalid)


instance PersistFieldSql Comic where
    sqlType _ = SqlInt64


instance ES.SqlString URI


instance PersistField URI where
    toPersistValue url = PersistText $ render url
    fromPersistValue (PersistText s) = first toErrorMsg $ mkURI s
      where
        toErrorMsg = ("converting to `URI` failed\n" <>) . T.pack . displayException
    fromPersistValue invalid =
        Left $ "reading `URI` failed, expected `PersistText`, received: " <> T.pack (show invalid)


instance PersistFieldSql URI where
    sqlType _ = SqlString


instance PersistField Domain where
    toPersistValue hostName = PersistText $ unRText hostName
    fromPersistValue (PersistText s) = first toErrorMsg $ mkHost s
      where
        toErrorMsg =
            ("converting to `RText 'Host` failed\n" <>) . T.pack . displayException
    fromPersistValue invalid =
        Left $ "reading `RText 'Host` failed, expected `PersistText`, received: " <> T.pack (show invalid)


instance PersistFieldSql Domain where
    sqlType _ = SqlString


instance ES.SqlString Domain


instance PersistField (RText 'Username) where
    toPersistValue hostName = PersistText $ unRText hostName
    fromPersistValue (PersistText s) = first toErrorMsg $ mkUsername s
      where
        toErrorMsg =
            ("converting to `RText 'Username` failed\n" <>) . T.pack . displayException
    fromPersistValue invalid =
        Left $ "reading `RText 'Username` failed, expected `PersistText`, received: " <> T.pack (show invalid)


instance PersistFieldSql (RText 'Username) where
    sqlType _ = SqlString


instance ES.SqlString (RText 'Username)


instance PersistField (RText 'Password) where
    toPersistValue hostName = PersistText $ unRText hostName
    fromPersistValue (PersistText s) = first toErrorMsg $ mkPassword s
      where
        toErrorMsg =
            ("converting to `RText 'Password` failed\n" <>) . T.pack . displayException
    fromPersistValue invalid =
        Left $ "reading `RText 'Password` failed, expected `PersistText`, received: " <> T.pack (show invalid)


instance PersistFieldSql (RText 'Password) where
    sqlType _ = SqlString


instance ES.SqlString (RText 'Password)


instance PersistField (Path Rel Dir) where
    toPersistValue relDir = PersistText $ T.pack $ toFilePath relDir
    fromPersistValue (PersistText s) = first toErrorMsg $ parseRelDir $ T.unpack s
      where
        toErrorMsg =
            ("converting to `Path Rel Dir` failed\n" <>) . T.pack . displayException
    fromPersistValue invalid =
        Left $ "reading `Path Rel Dir` failed, expected `PersistText`, received: " <> T.pack (show invalid)


instance PersistFieldSql (Path Rel Dir) where
    sqlType _ = SqlString


instance ES.SqlString (Path Rel Dir)


instance Show Volume where
    show (Volume volume) = show volume


instance PersistField Volume where
    toPersistValue (Volume n) = PersistInt64 $ fromIntegral n
    fromPersistValue (PersistInt64 n) = Right $ Volume $ fromIntegral n
    fromPersistValue invalid =
        Left $ "reading `Volume` failed, expected `PersistInt64`, received: " <> T.pack (show invalid)


instance PersistFieldSql Volume where
    sqlType _ = SqlInt64


instance PersistField Chapter where
    toPersistValue chapter = PersistText $ T.pack $ show chapter
    fromPersistValue (PersistText t) = first toErrorMsg $ mkChapter t
      where
        toErrorMsg =
            ("converting to `Chapter` failed\n" <>) . T.pack . displayException
    fromPersistValue invalid =
        Left $ "reading `Chapter` failed, expected `PersistText`, received: " <> T.pack (show invalid)


instance PersistFieldSql Chapter where
    sqlType _ = SqlString


instance ES.SqlString Chapter


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


    compare a b
        | a == b = EQ
        | a < b && b > a = LT
        | a > b && b < a = GT
        | otherwise =
            error $
                "comparing overlapping range: " <> show a <> " vs " <> show b


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
    web Web
    domain Domain
    username (RText 'Username) Maybe
    password (RText 'Password) Maybe
    sentinel URI
    Primary web
    deriving Eq Show
  Comics
    comic Comic Primary
    title Text
    folder (Path Rel Dir)
    volume Volume
    chapter Chapter
    Primary comic
    deriving Eq Show
  Urls
    web Web
    comic Comic
    path URI
    Primary web comic
    deriving Eq Show
|]
