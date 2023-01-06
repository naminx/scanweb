{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lib.Taggy (
    hasClass,
    hasClasses,
    notAttributed,
) where

import Control.Lens
import Data.Type.Equality (type (~))
import RIO
import qualified RIO.List as L (intersect)
import qualified RIO.Text as T (unwords, words)
import Text.Taggy.Lens (Element, attrs)


hasClass
    :: (Applicative f, IxValue m ~ Text, Index m ~ Text, Ixed m)
    => Text
    -> (() -> f ())
    -> m
    -> f m
hasClass c =
    ix "class" . contains_ c
  where
    contains_ :: Text -> Prism' Text ()
    contains_ c' = prism' (\() -> c') $ guard . (elem c' . T.words)


hasClasses
    :: (Applicative f, IxValue m ~ Text, Index m ~ Text, Ixed m)
    => [Text]
    -> (() -> f ())
    -> m
    -> f m
hasClasses cs =
    ix "class" . contains_ cs
  where
    contains_ :: [Text] -> Prism' Text ()
    contains_ cs' = prism' (\() -> T.unwords cs') $ guard . ((cs' ==) . L.intersect cs' . T.words)


notAttributed :: Fold (HashMap Text Text) a -> Traversal' Element Element
notAttributed prop = filtered . hasn't $ attrs . prop
