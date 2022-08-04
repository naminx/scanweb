{-# LANGUAGE NoImplicitPrelude #-}

-- | 'WOE' is an acronym for "Write-Once Enum". This package provides the 'IsoEnum' class to allow for more convenient declaration of arbitrary-index enums. This removes the need for writing boilerplate 'toEnum'/'fromEnum' definitions if you were to implement 'Enum' directly. To expose an 'Enum' interface for your custom enum type, simply do @deriving Enum via WOE@. This requires the @DerivingVia@ extension provided by GHC 8.6.1+.
module Data.WOE (
    IsoEnum,
    mapping,
    WOE (..),
    toEnumSafely,
    fromEnumSafely,
) where

import Data.Tuple (swap)
import RIO
import RIO.Partial (toEnum)


class Eq a => IsoEnum a where
    mapping :: [(Int, a)]


newtype WOE a = WOE {unwrapWOE :: a}


toEnumSafely :: IsoEnum a => Int -> Maybe a
toEnumSafely n = lookup n mapping


fromEnumSafely :: IsoEnum a => a -> Maybe Int
fromEnumSafely x = lookup x $ swap <$> mapping


instance IsoEnum a => Enum (WOE a) where
    toEnum = maybe (error "Invalid enum index.") WOE . toEnumSafely
    fromEnum = fromMaybe (error "Undefined enum index") . fromEnumSafely . unwrapWOE
