{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lib.Esqueleto where

import Database.Esqueleto.Experimental
import RIO (MonadUnliftIO, ReaderT, flip)


runSql
    :: forall backend m a
     . (MonadUnliftIO m, BackendCompatible SqlBackend backend)
    => backend
    -> ReaderT backend m a
    -> m a
runSql = flip runSqlConn
