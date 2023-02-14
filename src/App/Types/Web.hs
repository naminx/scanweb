{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module App.Types.Web where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Database.Esqueleto.Experimental (
    PersistField (..),
    PersistFieldSql (..),
    PersistValue (PersistInt64),
    SqlType (SqlInt64),
 )
import Lib
import qualified RIO.Text as T (pack)
import Web.Internal.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))
import Web.PathPieces (PathPiece (..))


newtype Web = Web {unWeb :: Int}
    deriving (Eq, Ord, Show, Read)
    deriving newtype
        ( FromJSON
        , ToJSON
        , PathPiece
        , ToHttpApiData
        , FromHttpApiData
        )


instance PersistField Web where
    toPersistValue (Web n) = PersistInt64 $ fromIntegral n
    fromPersistValue (PersistInt64 n) = Right $ Web $ fromIntegral n
    fromPersistValue invalid =
        Left $
            "reading `Web` failed"
                <> ", expected `PersistInt64`"
                <> (", received: " <> T.pack (show invalid))


instance PersistFieldSql Web where
    sqlType _ = SqlInt64


mangaRawSo :: Web
mangaRawSo = Web 0


mangaRawIo :: Web
mangaRawIo = Web 1


manga1001Su :: Web
manga1001Su = Web 2


weLoMaArt :: Web
weLoMaArt = Web 3


weLoveMangaOne :: Web
weLoveMangaOne = Web 4


klMangaNet :: Web
klMangaNet = Web 5


hachiMangaCom :: Web
hachiMangaCom = Web 6


j8JpCom :: Web
j8JpCom = Web 7
