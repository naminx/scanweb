{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lib.Console
    ( vivid
    , dull
    , resetSGR
    ) where

import RIO
import qualified RIO.Text as T (pack)
import System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleLayer (..), SGR (..))
import System.Console.ANSI.Codes (setSGRCode)


vivid :: Color -> Text
vivid color = T.pack $ setSGRCode [Reset, SetColor Foreground Vivid color]


dull :: Color -> Text
dull color = T.pack $ setSGRCode [Reset, SetColor Foreground Dull color]


resetSGR :: Text
resetSGR = T.pack $ setSGRCode [Reset]
