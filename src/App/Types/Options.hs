{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module App.Types.Options where

import App.Types.AppMode
import App.Types.Path
import Control.Lens (makeFieldsNoPrefix)
import Lib
import Path (Abs, File, SomeBase)


data Options = Options
    { _rootDir :: !(Path Abs Dir)
    , _dbFile :: !(SomeBase File)
    , _appMode :: !AppMode
    , _maxNumPages :: !Int
    }
    deriving (Eq, Show)


makeFieldsNoPrefix ''Options
