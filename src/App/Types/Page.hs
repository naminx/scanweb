{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module App.Types.Page where

import Lib


newtype Page = Page Int
    deriving (Enum, Eq, Ord, Show)
