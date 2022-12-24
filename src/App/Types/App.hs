{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module App.Types.App where

import App.Types.Comic
import App.Types.Misc
import App.Types.Options
import App.Types.Page
import App.Types.ReleaseInfo
import App.Types.URI
import App.Types.Web
import Control.Lens (makeClassy)
import Database.Esqueleto.Experimental (SqlBackend)
import Lib
import RIO.Process (HasProcessContext (processContextL), ProcessContext)
import Web.Api.WebDriver (ContextId, SessionId)


data App = App
    { _logFunc :: !LogFunc
    , _processContext :: !ProcessContext
    , _options :: !Options
    , -- Add other app-specific configuration information here
      _currentWeb :: !Web
    , _currentPage :: !Page
    , _currentSqlBackend :: !SqlBackend
    , _webTable :: WebTable
    , _domainTable :: DomainTable
    , _comicTable :: ComicTable
    , _urlTable :: UrlTable
    , _currentWdSession :: SessionId
    , _currentWebInfo :: !WebInfo
    , _currentComic :: !Comic
    , _currentComicUrl :: !URI
    , _currentComicInfo :: !ComicInfo
    , _currentComicWindow :: !ContextId
    , _currentReferer :: !URI
    , _newReleaseInfo :: !(Maybe ReleaseInfo)
    , _stickyLine :: !Text
    }


makeClassy ''App


instance HasLogFunc App where
    logFuncL = logFunc


instance HasProcessContext App where
    processContextL = processContext
