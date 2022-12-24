{-# LANGUAGE NoImplicitPrelude #-}

module App.Types.ReleaseInfo where

import App.Types.Chapter
import App.Types.Volume
import Lib


data ReleaseInfo
    = Episode Chapter
    | Episodes (Chapter, Chapter)
    | Book Volume
    deriving (Eq, Show)


instance Ord ReleaseInfo where
    Episode a < Episode b = a < b
    Episode a < Episodes (b, _) = a < b
    Episodes _ < Book _ = False
    Episodes (_, a) < Episode b = a < b
    Episodes (_, a) < Episodes (b, _) = a < b
    Episode _ < Book _ = False
    Book _ < Episode _ = True
    Book _ < Episodes _ = True
    Book a < Book b = a < b


    Episode a > Episode b = a > b
    Episode a > Episodes (_, b) = a > b
    Episodes _ > Book _ = False
    Episodes (a, _) > Episode b = a > b
    Episodes (a, _) > Episodes (_, b) = a > b
    Episode _ > Book _ = False
    Book _ > Episode _ = True
    Book _ > Episodes _ = True
    Book a > Book b = a > b


    compare a b
        | a == b = EQ
        | a < b && b > a = LT
        | a > b && b < a = GT
        | otherwise =
            error $
                "comparing overlapping range: "
                    <> (show a <> " vs " <> show b)
