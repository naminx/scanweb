{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.WebDriver where

import Control.Lens (makeLenses)
import Data.Default
import GHC.Generics hiding (R, S)
import RIO (Ord (..))
import Web.Api.WebDriver


instance Ord ContextId where
    compare a b = compare (theContextId a) (theContextId b)


deriving instance Generic ChromeOptions
deriving instance Default ChromeOptions


makeLenses ''Capabilities
makeLenses ''ChromeOptions
makeLenses ''LogOptions
makeLenses ''R
makeLenses ''S
makeLenses ''WDEnv
makeLenses ''WDState
makeLenses ''WebDriverConfig
