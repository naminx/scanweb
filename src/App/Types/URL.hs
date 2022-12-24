{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module App.Types.URL where

import Control.Lens
import Lib
import qualified RIO.Text as T (intercalate, unpack)
import Text.URI (URI, render, unRText)
import Text.URI.Lens hiding (unRText)


newtype URL = URL {getURI :: URI}
    deriving (Eq)


instance Show URL where
    show (URL url) = T.unpack $ renderUrl url
      where
        renderUrl u =
            "URL \""
                <> render (u & uriPath .~ [] & uriQuery .~ [] & uriFragment .~ Nothing)
                <> (u ^. uriAuthority . to (isRight >>> trueToSlash))
                <> (u ^.. uriPath . each . to unRText & T.intercalate "/")
                <> (u ^. uriTrailingSlash . to trueToSlash)
                <> render (u & uriScheme .~ Nothing & uriAuthority .~ Left False & uriPath .~ [])
                <> "\""

        trueToSlash = bool "" "/"
