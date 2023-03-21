{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module App.Types.Path
    ( Path
    , Rel
    , Dir
    ) where

import Database.Esqueleto.Experimental
    ( PersistField (..)
    , PersistFieldSql (..)
    , PersistValue (PersistText)
    , SqlType (SqlString)
    )
import qualified Database.Esqueleto.Internal.Internal as ES (SqlString)
import Lib
import Path (Dir, Path, Rel, parseRelDir, toFilePath)
import qualified RIO.Text as T (pack, unpack)


instance PersistField (Path Rel Dir) where
    toPersistValue relDir = PersistText $ T.pack $ toFilePath relDir
    fromPersistValue (PersistText s) =
        first toErrorMsg $ parseRelDir $ T.unpack s
      where
        toErrorMsg =
            ("converting to `Path Rel Dir` failed\n" <>)
                . T.pack
                . displayException
    fromPersistValue invalid =
        Left $
            "reading `Path Rel Dir` failed"
                <> ", expected `PersistText`"
                <> (", received: " <> T.pack (show invalid))


instance PersistFieldSql (Path Rel Dir) where
    sqlType _ = SqlString


instance ES.SqlString (Path Rel Dir)
