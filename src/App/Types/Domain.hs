{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module App.Types.Domain
    ( Domain
    ) where

import Database.Esqueleto.Experimental
    ( PersistField (..)
    , PersistFieldSql (..)
    , PersistValue (PersistText)
    , SqlType (SqlString)
    )
import qualified Database.Esqueleto.Internal.Internal as ES (SqlString)
import Lib
import qualified RIO.Text as T (pack)
import Text.URI (mkHost, unRText)


instance PersistField Domain where
    toPersistValue hostName = PersistText $ unRText hostName
    fromPersistValue (PersistText s) = first toErrorMsg $ mkHost s
      where
        toErrorMsg =
            ("converting to `RText 'Host` failed\n" <>)
                . T.pack
                . displayException
    fromPersistValue invalid =
        Left $
            "reading `RText 'Host` failed"
                <> ", expected `PersistText`"
                <> (", received: " <> T.pack (show invalid))


instance PersistFieldSql Domain where
    sqlType _ = SqlString


instance ES.SqlString Domain
