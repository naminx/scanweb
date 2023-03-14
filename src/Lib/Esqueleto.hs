{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lib.Esqueleto where

import Database.Esqueleto.Experimental as ES
import Database.Esqueleto.Internal.Internal
import RIO


#if MIN_VERSION_GLASGOW_HASKELL(9,2,0,0)
#else
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Internal.Builder as TLB
import qualified RIO.Text.Lazy as TL
#endif


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

#if MIN_VERSION_GLASGOW_HASKELL(9,2,0,0)
#else

values ::
    (ToSomeValues a, ES.ToAliasReference a, ES.ToAlias a) =>
    NE.NonEmpty a ->
    ES.From a
values exprs = ES.From $ do
    ident <- newIdentFor $ DBName "vq"
    alias <- ES.toAlias $ NE.head exprs
    ref <- ES.toAliasReference ident alias
    let aliasIdents =
            mapMaybe
                (\(SomeValue (ERaw aliasMeta _)) -> sqlExprMetaAlias aliasMeta)
                $ toSomeValues ref
    pure (ref, const $ mkExpr ident aliasIdents)
  where
    someValueToSql :: IdentInfo -> SomeValue -> (TLB.Builder, [PersistValue])
    someValueToSql info (SomeValue expr) = materializeExpr info expr

    mkValuesRowSql :: IdentInfo -> [SomeValue] -> (TLB.Builder, [PersistValue])
    mkValuesRowSql info vs =
        let materialized = someValueToSql info <$> vs
            valsSql = TLB.toLazyText . fst <$> materialized
            params = concatMap snd materialized
         in (TLB.fromLazyText $ "(" <> TL.intercalate "," valsSql <> ")", params)

    -- (VALUES (v11, v12,..), (v21, v22,..)) as "vq"("v1", "v2",..)
    mkExpr :: Ident -> [Ident] -> IdentInfo -> (TLB.Builder, [PersistValue])
    mkExpr valsIdent colIdents info =
        let materialized = mkValuesRowSql info . toSomeValues <$> NE.toList exprs
            (valsSql, params) =
                ( TL.intercalate "," $ map (TLB.toLazyText . fst) materialized
                , concatMap snd materialized
                )
            colsAliases =
                TL.intercalate "," (map (TLB.toLazyText . useIdent info) colIdents)
         in ( "(VALUES " <> TLB.fromLazyText valsSql <> ") AS "
                <> useIdent info valsIdent
                <> ("(" <> TLB.fromLazyText colsAliases <> ")")
            , params
            )

#endif
