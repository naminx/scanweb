{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module App.Types.RText
    ( RText
    , RTextLabel (Username, Password)
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
import Text.URI (RText, RTextLabel (..), mkPassword, mkUsername, unRText)


instance PersistField (RText 'Username) where
    toPersistValue hostName = PersistText $ unRText hostName
    fromPersistValue (PersistText s) = first toErrorMsg $ mkUsername s
      where
        toErrorMsg =
            ("converting to `RText 'Username` failed\n" <>)
                . T.pack
                . displayException
    fromPersistValue invalid =
        Left $
            "reading `RText 'Username` failed"
                <> ", expected `PersistText`"
                <> (", received: " <> T.pack (show invalid))


instance PersistFieldSql (RText 'Username) where
    sqlType _ = SqlString


instance ES.SqlString (RText 'Username)


instance PersistField (RText 'Password) where
    toPersistValue hostName = PersistText $ unRText hostName
    fromPersistValue (PersistText s) = first toErrorMsg $ mkPassword s
      where
        toErrorMsg =
            ("converting to `RText 'Password` failed\n" <>)
                . T.pack
                . displayException
    fromPersistValue invalid =
        Left $
            "reading `RText 'Password` failed"
                <> ", expected `PersistText`"
                <> (", received: " <> T.pack (show invalid))


instance PersistFieldSql (RText 'Password) where
    sqlType _ = SqlString


instance ES.SqlString (RText 'Password)
