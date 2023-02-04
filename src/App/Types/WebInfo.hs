{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module App.Types.WebInfo where

import App.Types.Domain
import Control.Lens (makeClassy)
import RIO
import Text.URI
import Text.URI.QQ (host)


data WebInfo = WebInfo
    { _webDomain :: Domain
    , _userInfo :: Maybe UserInfo
    , _sentinel :: URI
    , _genUrl :: Text
    , _isLoaded :: Text
    , _scrapeComics :: Text
    , _scrapeLatest :: Text
    , _scrapeChapters :: Text
    , _scrapeImages :: Text
    }


makeClassy ''WebInfo


emptyWebInfo :: WebInfo
emptyWebInfo =
    WebInfo
        { _webDomain = [host||]
        , _userInfo = Nothing
        , _sentinel = emptyURI
        , _genUrl = ""
        , _isLoaded = ""
        , _scrapeComics = ""
        , _scrapeLatest = ""
        , _scrapeChapters = ""
        , _scrapeImages = ""
        }
