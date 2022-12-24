{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module App.Types.URI (
    URI,
) where

import Database.Esqueleto.Experimental (
    PersistField (..),
    PersistFieldSql (..),
    PersistValue (PersistText),
    SqlType (SqlString),
 )
import qualified Database.Esqueleto.Internal.Internal as ES (SqlString)
import Lib
import qualified RIO.Text as T (pack)
import Text.URI (URI, mkURI, render)


instance PersistField URI where
    toPersistValue url = PersistText $ render url
    fromPersistValue (PersistText s) = first toErrorMsg $ mkURI s
      where
        toErrorMsg =
            ("converting to `URI` failed\n" <>)
                . T.pack
                . displayException
    fromPersistValue invalid =
        Left $
            "reading `URI` failed"
                <> ", expected `PersistText`"
                <> (", received: " <> T.pack (show invalid))


instance PersistFieldSql URI where
    sqlType _ = SqlString


instance ES.SqlString URI
