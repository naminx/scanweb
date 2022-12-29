{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module App.Types.Web where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..))
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Scientific (toBoundedInteger)
import Data.WOE (IsoEnum (..), WOE (..), fromEnumSafely, toEnumSafely)
import Database.Esqueleto.Experimental (
    PersistField (..),
    PersistFieldSql (..),
    PersistValue (PersistInt64),
    SqlType (SqlInt64),
 )
import Lib
import qualified RIO.Text as T (pack)
import Web.Internal.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))
import Web.PathPieces (PathPiece (..), readFromPathPiece, showToPathPiece)


data Web
    = MangaRawSo
    | RawDevArtCom
    | WeLoMaArt
    | WeLoveMangaOne
    | KlMangaNet
    | HachiMangaCom
    | J8JpCom
    | MangaRawIo
    | MangaGunCom
    | Manga1001Su
    deriving (Bounded, Eq, Generic, Ord, Read, Show)
    deriving (Enum) via WOE Web


instance IsoEnum Web where
    mapping =
        [ (0, MangaRawSo)
        , (1, RawDevArtCom)
        , (2, WeLoMaArt)
        , (3, WeLoveMangaOne)
        , (4, KlMangaNet)
        , (5, HachiMangaCom)
        , (6, J8JpCom)
        , (7, MangaRawIo)
        , (8, MangaGunCom)
        , (9, Manga1001Su)
        ]


instance Hashable Web


instance ToJSON Web where
    toJSON = Number . fromIntegral . fromMaybe raiseError . fromEnumSafely
      where
        raiseError =
            error $
                "internal error:"
                    <> " missing value in IsoEnum mapping for Web type"


instance FromJSON Web where
    parseJSON (Number n) = case toBoundedInteger n >>= toEnumSafely of
        Just webNo ->
            pure webNo
        Nothing ->
            fail $
                "parsing Web failed"
                    <> (", value out of bound: " <> show n)
    parseJSON invalid =
        prependFailure
            "parsing Web failed, "
            (typeMismatch "Number" invalid)


instance PathPiece Web where
    toPathPiece = showToPathPiece . fromMaybe raiseError . fromEnumSafely
      where
        raiseError =
            error $
                "internal error:"
                    <> " missing value in IsoEnum mapping for Web type"
    fromPathPiece = readFromPathPiece >=> toEnumSafely


instance ToHttpApiData Web where
    toUrlPiece = toPathPiece


instance FromHttpApiData Web where
    parseUrlPiece x =
        parseUrlPiece x >>= maybe leftValue Right . toEnumSafely
      where
        leftValue = Left "Web value out of bound"


instance PersistField Web where
    toPersistValue =
        PersistInt64
            . fromIntegral
            . fromMaybe raiseError
            . fromEnumSafely
      where
        raiseError =
            error $
                "internal error:"
                    <> " missing value in IsoEnum mapping for Web type"
    fromPersistValue (PersistInt64 n) =
        maybe toErrorMsg Right $ toEnumSafely $ fromIntegral n
      where
        toErrorMsg =
            Left $
                "converting to Web failed"
                    <> (", value out of bound: " <> T.pack (show n))
    fromPersistValue invalid =
        Left $
            "reading Web failed"
                <> ", expected PersistInt64"
                <> (", received: " <> T.pack (show invalid))


instance PersistFieldSql Web where
    sqlType _ = SqlInt64
