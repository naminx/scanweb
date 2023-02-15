{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lib.Esqueleto where

import Database.Esqueleto.Experimental as ES
import Database.Esqueleto.Internal.Internal (Update)
import RIO (MonadUnliftIO, ReaderT, flip)


runSql ::
    forall backend m a.
    (MonadUnliftIO m, BackendCompatible SqlBackend backend) =>
    backend ->
    ReaderT backend m a ->
    m a
runSql = flip runSqlConn


set_ ::
    PersistEntity val =>
    SqlExpr (Entity val) ->
    [SqlExpr (Entity val) -> SqlExpr Update] ->
    SqlQuery ()
set_ = ES.set


(.^) ::
    forall typ val.
    (PersistEntity val, PersistField typ) =>
    SqlExpr (Entity val) ->
    EntityField val typ ->
    SqlExpr (ES.Value typ)
(.^) = (ES.^.)
infixl 8 .^
