{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module App.Types.Misc where

import App.Types.Chapter
import App.Types.Comic
import App.Types.Domain
import App.Types.Path
import App.Types.Title
import App.Types.URI
import App.Types.Volume
import App.Types.Web
import Lib
import Path (reldir)
import Text.URI (UserInfo, emptyURI)
import Text.URI.QQ (host)


type WebInfo = (Domain, Maybe UserInfo, URI)
type WebTable = Map Web WebInfo
type DomainTable = Map Domain Web
type ComicInfo = (Title, Path Rel Dir, Volume, Chapter)
type ComicTable = Map Comic ComicInfo
type UrlTable = Map URI (Web, Comic)


emptyWebInfo :: WebInfo
emptyWebInfo = ([host||], Nothing, emptyURI)


emptyComicInfo :: ComicInfo
emptyComicInfo = (Title "", [reldir|.|], Volume 0, emptyChapter)
