{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lib.MIME where

import Control.Lens
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Base64 (base64)
import Data.ByteString.Base64 (decodeBase64Lenient)
import Data.IMF.Syntax (original)
import Data.MIME (ContentType, ctSubtype, ctType, parseContentType)
import qualified Data.Text.Encoding as T (decodeUtf8)
import Data.Word8 (toLower)
import Lib.RIO
import qualified RIO.ByteString as BS


contentTypeToExtension :: ContentType -> Text
contentTypeToExtension ct = case ct ^. ctType of
    "image" -> case ct ^. ctSubtype of
        "jpeg" -> "jpg"
        _ -> T.decodeUtf8 $ BS.map toLower $ original $ ct ^. ctSubtype
    _ -> ""


parseEmbeddedData :: Parser (Text, ByteString)
parseEmbeddedData = do
    contentType_ <- string "data:" >> parseContentType >>= (string ";" >>) . return
    base64data <- string "base64," >> base64
    return (contentTypeToExtension contentType_, decodeBase64Lenient base64data)
