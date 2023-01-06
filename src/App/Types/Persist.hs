{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wmissed-extra-shared-lib #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module App.Types.Persist where

import App.Types.Chapter
import App.Types.Comic
import App.Types.Domain
import App.Types.Path
import App.Types.RText
import App.Types.Title
import App.Types.URI
import App.Types.Volume
import App.Types.Web
import Database.Persist.TH (mkPersist, persistLowerCase, share, sqlSettings)
import Lib


share
    [mkPersist sqlSettings]
    [persistLowerCase|
  Webs
    web Web
    domain Domain
    username (RText 'Username) Maybe
    password (RText 'Password) Maybe
    sentinel URI
    Primary web
    deriving Eq Show
  Comics
    comic Comic Primary
    title Title
    folder (Path Rel Dir)
    volume Volume
    chapter Chapter
    Primary comic
    deriving Eq Show
  Urls
    web Web
    comic Comic
    path URI
    Primary web comic
    deriving Eq Show
|]
