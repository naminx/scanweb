{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module App.Types.Comic where

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


newtype Comic = Comic {unComic :: Int}
    deriving (Eq, Ord, Show, Read)
    deriving newtype
        ( FromJSON
        , ToJSON
        , PathPiece
        , ToHttpApiData
        , FromHttpApiData
        )


instance PersistField Comic where
    toPersistValue (Comic n) = PersistInt64 $ fromIntegral n
    fromPersistValue (PersistInt64 n) = Right $ Comic $ fromIntegral n
    fromPersistValue invalid =
        Left $
            "reading `Comic` failed"
                <> ", expected `PersistInt64`"
                <> (", received: " <> T.pack (show invalid))


instance PersistFieldSql Comic where
    sqlType _ = SqlInt64
