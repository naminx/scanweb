{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module App.Types.Title where

import Database.Esqueleto.Experimental
    ( PersistField (..)
    , PersistFieldSql (..)
    , PersistValue (PersistText)
    , SqlType (SqlString)
    )
import Lib
import qualified RIO.Text as T (pack)


newtype Title = Title {unTitle :: Text}
    deriving (Eq, Ord, Show)


instance PersistField Title where
    toPersistValue (Title t) = PersistText t
    fromPersistValue (PersistText t) = Right $ Title t
    fromPersistValue invalid =
        Left $
            "reading `Title` failed"
                <> ", expected `PersistText`"
                <> (", received: " <> T.pack (show invalid))


instance PersistFieldSql Title where
    sqlType _ = SqlString
