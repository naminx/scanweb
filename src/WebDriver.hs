{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module WebDriver where


{--
import App.Types (HasApp (..))
import Control.Lens ((.=), (<%=))
import RIO

-- for `MonadBase` and `MonadBaseControl` instance
import RIO.Orphans ()
import Test.WebDriver.Class (WebDriver, doCommand)
import Test.WebDriver.Internal (getJSONResult, mkRequest, sendHTTPRequest)
import Test.WebDriver.Session (WDSessionState (..))

instance forall env s. (HasStateRef s env, HasApp s) => WDSessionState (RIO env) where
    getSession =
        currentWdSession <%= id
    putSession wdSess =
        currentWdSession .= wdSess

instance (HasStateRef s env, HasApp s) => WebDriver (RIO env) where
    doCommand method path args =
        mkRequest method path args
            >>= sendHTTPRequest
            >>= either throwIO return
            >>= getJSONResult
            >>= either throwIO return
--}
