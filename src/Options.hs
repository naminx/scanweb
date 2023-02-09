{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Options where

import App.Config
import App.Exceptions
import Control.Arrow (left)
import Data.WOE (toEnumSafely)
import Import
import Options.Applicative.Simple
import Path (SomeBase (Rel), parseAbsDir, parseSomeFile)
import RIO.Text (pack)
import Text.Megaparsec (sepBy, single)
import qualified Text.Megaparsec as MP (try)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.URI (mkURI)


---
timestamp :: IsString s => s
timestamp = __TIMESTAMP__


parseOptions :: IO Options
parseOptions = do
    (cliOptions, ()) <-
        simpleOptions
            timestamp
            ("Scanweb: " <> timestamp)
            "Download comics from a selection of web sites."
            ( Options
                <$> option
                    (eitherReader $ left displayException . parseAbsDir)
                    ( short 'r'
                        <> long "root"
                        <> metavar "DIR"
                        <> value defaultRootDir
                        <> showDefault
                        <> help "Full path to root directory of comics"
                    )
                <*> option
                    (eitherReader $ left displayException . parseSomeFile)
                    ( short 'b'
                        <> long "database"
                        <> metavar "FILE"
                        <> value (Rel defaultDbFileName)
                        <> showDefault
                        <> help "Full path to database file or relative path from root directory of comics"
                    )
                <*> ( modeScanWebs
                        <|> modeUpdateComic
                        <|> modeDownloadRelease
                        <|> modeDownloadAddress
                        <|> modeListWebs
                        <|> modeListComics
                        <|> modeFallback
                    )
                <*> option
                    auto
                    ( short 'm'
                        <> long "max"
                        <> metavar "PAGES"
                        <> value defaultMaxNumPages
                        <> showDefault
                        <> help "Maximum number of pages to scan"
                    )
            )
            empty
    return cliOptions


modeScanWebs :: Parser AppMode
modeScanWebs =
    ScanWebs
        <$> option
            (eitherReader (left displayException . mkWebList . pack))
            ( short 's'
                <> long "scan"
                <> metavar "WEBs"
                <> value
                    [ WeLoMaArt
                    , WeLoveMangaOne
                    , KlMangaNet
                    ]
                <> help
                    ( "Scan specified webs (default mode), such as 1-3,5,7-9 "
                        <> "(-w for list of known webs)"
                    )
            )


modeUpdateComic :: Parser AppMode
modeUpdateComic =
    UpdateComic
        <$> option
            (eitherReader (left displayException . mkTupleWebComicMaybeRelInfo . pack))
            ( short 'u'
                <> long "update"
                <> metavar "WEB:COMIC[:VOLUME|::CHAPTER[-CHAPTER]]"
                <> help
                    ( "Update COMIC No. (-c for list of known comics) from "
                        <> "WEB No. (-w for list of known webs). "
                        <> "Latest VOLUME/CHAPTER needed if web not provide."
                    )
            )


modeDownloadRelease :: Parser AppMode
modeDownloadRelease =
    DownloadRelease
        <$> option
            (eitherReader (left displayException . mkTupleWebComicRelInfo . pack))
            ( short 'd'
                <> long "download"
                <> metavar "WEB:COMIC[:VOLUME|::CHAPTER[-CHAPTER]]"
                <> help
                    ( "Download VOLUME/CHAPTER of COMIC No. (-c for list of "
                        <> "known comics) from WEB No. (-w for list of known webs)"
                    )
            )


modeDownloadAddress :: Parser AppMode
modeDownloadAddress =
    DownloadAddress
        <$> option
            (eitherReader (left displayException . mkURI . pack))
            ( short 'a'
                <> long "address"
                <> metavar "URI"
                <> help "Download VOLUME/CHAPTER at a specific address"
            )


mkTupleWebComicMaybeRelInfo :: Text -> Either SomeException (Web, Comic, Maybe ReleaseInfo)
mkTupleWebComicMaybeRelInfo = parseEither $ do
    _ <- optional (single ' ')
    web <- singleWeb
    _ <- single ':'
    comic <- decimal
    relInfo <- optional releaseInfo
    return (web, Comic comic, relInfo)


mkTupleWebComicRelInfo :: Text -> Either SomeException (Web, Comic, ReleaseInfo)
mkTupleWebComicRelInfo = parseEither $ do
    _ <- optional (single ' ')
    web <- singleWeb
    _ <- single ':'
    comic <- decimal
    relInfo <- releaseInfo
    return (web, Comic comic, relInfo)


releaseInfo :: TextParser ReleaseInfo
releaseInfo = MP.try episodes <|> MP.try episode <|> book
  where
    book = do
        _ <- single ':'
        Book . Volume <$> decimal
    episode = do
        _ <- single ':'
        _ <- single ':'
        Episode <$> comicChapter
    episodes = do
        _ <- single ':'
        _ <- single ':'
        chapterBegin <- comicChapter
        _ <- single '-'
        chapterEnd <- comicChapter
        return $ Episodes (chapterBegin, chapterEnd)


modeListWebs :: Parser AppMode
modeListWebs =
    flag'
        ListWebs
        ( short 'w'
            <> long "webs"
            <> help "Show list of known webs"
        )


modeListComics :: Parser AppMode
modeListComics =
    flag'
        ListComics
        ( short 'c'
            <> long "comics"
            <> help "Show list of known comics"
        )


modeFallback :: Parser AppMode
modeFallback =
    flag
        (ScanWebs [minBound :: Web .. maxBound :: Web])
        (ScanWebs [minBound :: Web .. maxBound :: Web])
        mempty


singleWeb :: TextParser Web
singleWeb = do
    webNo <- decimal
    case toEnumSafely webNo of
        Nothing -> fail $ displayException $ InvalidWebNo webNo
        Just web -> return web


webRange :: TextParser [Web]
webRange = do
    firstWeb <- singleWeb
    _ <- single '-'
    lastWeb <- singleWeb
    return [firstWeb .. lastWeb]


webList :: TextParser [Web]
webList = do
    fmap concat $ (MP.try webRange <|> fmap (: []) singleWeb) `sepBy` single ','


mkWebList :: Text -> Either SomeException [Web]
mkWebList = parseEither webList
