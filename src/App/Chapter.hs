{-# LANGUAGE NoImplicitPrelude #-}

module App.Chapter (
    Chapter (..),
    comicChapter,
    emptyChapter,
    mkChapter,
) where

import RIO
import Text.Megaparsec (ParsecT, eof, runParser, single)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.URI (ParseException (ParseException))


type Parser m a = ParsecT Void Text m a


data Chapter = Chapter Int (Maybe Int)
    deriving (Eq)


emptyChapter :: Chapter
emptyChapter = Chapter 0 Nothing


instance Ord Chapter where
    compare (Chapter x1 x2) (Chapter y1 y2) =
        if x1 /= y1
            then compare x1 y1
            else case (x2, y2) of
                (Nothing, Nothing) -> EQ
                (Nothing, Just _) -> LT
                (Just _, Nothing) -> GT
                (Just x3, Just y3) -> compare x3 y3


instance Show Chapter where
    show (Chapter c d) = case d of
        Nothing -> show c
        Just e -> show c <> "." <> show e


mkChapter :: MonadThrow m => Text -> m Chapter
mkChapter chapter = case runParser (comicChapter <* eof) "" chapter of
    Left err -> throwM $ ParseException err
    Right chapter' -> return chapter'


comicChapter :: Parser m Chapter
comicChapter = do
    chapter <- decimal
    section <- optional $ single '.' >> decimal
    return $ Chapter chapter section
