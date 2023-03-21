{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module App.Types.ReleaseInfo
    ( ReleaseInfo (..)
    , comicReleaseInfo
    , mkReleaseInfo
    ) where

import App.Types.Chapter
import App.Types.Volume
import Lib
import Text.Megaparsec (ParsecT)
import qualified Text.Megaparsec as MP (try)
import Text.Megaparsec.Char (string)
import Text.Megaparsec.Char.Lexer (decimal)


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


type Parser m a = ParsecT Void Text m a


comicReleaseInfo :: Parser m ReleaseInfo
comicReleaseInfo = MP.try episode <|> MP.try book <|> episodes
  where
    episode = do
        _ <- string "第"
        chap <- comicChapter
        _ <- string "話"
        return $ Episode chap

    book = do
        _ <- string "第"
        vol <- decimal
        _ <- string "巻"
        return $ Book $ Volume vol

    episodes = do
        _ <- string "第"
        beginChap <- comicChapter
        _ <- string "-"
        endChap <- comicChapter
        _ <- string "話"
        return $ Episodes (beginChap, endChap)


mkReleaseInfo :: Text -> Try ReleaseInfo
mkReleaseInfo = parseEither comicReleaseInfo
