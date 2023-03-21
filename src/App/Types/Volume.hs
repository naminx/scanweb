{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module App.Types.Volume where

import Database.Esqueleto.Experimental
    ( PersistField (..)
    , PersistFieldSql (..)
    , PersistValue (PersistInt64)
    , SqlType (SqlInt64)
    )
import Lib
import qualified RIO.Text as T (pack)


newtype Volume = Volume {unVolume :: Int} deriving (Eq, Ord)


instance Show Volume where
    show (Volume volume) = show volume


instance PersistField Volume where
    toPersistValue (Volume n) = PersistInt64 $ fromIntegral n
    fromPersistValue (PersistInt64 n) = Right $ Volume $ fromIntegral n
    fromPersistValue invalid =
        Left $
            "reading `Volume` failed"
                <> ", expected `PersistInt64`"
                <> (", received: " <> T.pack (show invalid))


instance PersistFieldSql Volume where
    sqlType _ = SqlInt64
