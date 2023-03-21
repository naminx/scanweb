{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lib.Extra
    ( Try
    , exclude
    , maybeToTry
    , sequencePair
    ) where

import Data.Either.Extra (maybeToEither)
import RIO
import qualified RIO.Set as S (fromList, toList, (\\))


type Try = Either SomeException


maybeToTry :: Exception e => e -> Maybe b -> Try b
maybeToTry = maybeToEither . toException


exclude :: Ord a => [a] -> [a] -> [a]
exclude x y =
    S.toList $ S.fromList x S.\\ S.fromList y


sequencePair :: Applicative f => (f a, f b) -> f (a, b)
sequencePair = uncurry $ liftA2 (,)
