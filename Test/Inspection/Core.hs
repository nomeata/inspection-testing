-- | This module implements some of analyses of Core expressions necessary for
-- "Test.Inspection". Normally, users of this pacakge can ignore this module. 
{-# LANGUAGE CPP, FlexibleContexts #-}
module Test.Inspection.Core
  ( slice
  , pprSlice
  , pprSliceDifference
  , eqSlice
  , freeOfType
  , doesNotAllocate
  ) where

import CoreSyn
import CoreUtils
import TyCoRep
import Type
import Var
import Id
import Name
import VarEnv
import Outputable
import PprCore
import Coercion
import Util

import qualified Data.Set as S
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Data.Maybe

type Slice = [(Var, CoreExpr)]

-- | Selects those bindings that define the given variable
slice :: [(Var, CoreExpr)] -> Var -> Slice
slice binds v = [(v,e) | (v,e) <- binds, v `S.member` used ]
  where
    used = execState (goV v) S.empty

    local = S.fromList (map fst binds)
    goV v | v `S.member` local = do
        seen <- gets (v `S.member`)
        unless seen $ do
            modify (S.insert v)
            let Just e = lookup v binds
            go e
          | otherwise = return ()

    go (Var v)                     = goV v
    go (Lit _ )                    = pure ()
    go (App e arg) | isTyCoArg arg = go e
    go (App e arg)                 = go e >> go arg
    go (Lam b e) | isTyVar b       = go e
    go (Lam _ e)                   = go e
    go (Let bind body)             = mapM_ go (rhssOfBind bind) >> go body
    go (Case s _ _ alts)           = go s >> mapM_ goA alts
    go (Cast e _)                  = go e
    go (Tick _ e)                  = go e
    go (Type _)                    = pure ()
    go (Coercion _)                = pure ()

    goA (_, _, e) = go e

-- | Pretty-print a slice
pprSlice :: Slice -> SDoc
pprSlice slice = withLessDetail $ pprCoreBindings [ NonRec v e  | (v,e) <- slice ]

-- | Pretty-print two slices, after removing variables occurring in both
pprSliceDifference :: Slice -> Slice -> SDoc
pprSliceDifference slice1 slice2 =
    nest 4 (hang (text "LHS" <> colon) 4 (pprSlice slice1')) $$
    nest 4 (hang (text "RHS" <> colon) 4 (pprSlice slice2'))
  where
    both = S.intersection (S.fromList (map fst slice1)) (S.fromList (map fst slice2))
    slice1' = filter (\(v,_) -> v `S.notMember` both) slice1
    slice2' = filter (\(v,_) -> v `S.notMember` both) slice2

withLessDetail :: SDoc -> SDoc
#if MIN_VERSION_GLASGOW_HASKELL(8,2,0,0)
withLessDetail sdoc = sdocWithDynFlags $ \dflags ->
     withPprStyle (defaultUserStyle dflags) sdoc
#else
withLessDetail sdoc = withPprStyle defaultUserStyle sdoc
#endif

type VarPair = (Var, Var)
type VarPairSet = S.Set VarPair

-- | This is a heuristic, which only works if both slices
-- have auxillary variables in the right order.
-- (This is mostly to work-around the buggy CSE in GHC-8.0)
-- It also breaks if there is shadowing.
eqSlice :: Bool {- ^ ignore types -} -> Slice -> Slice -> Bool
eqSlice _ slice1 slice2 | null slice1 || null slice2 = null slice1 == null slice2
  -- Mostly defensive programming (slices should not be empty)
eqSlice it slice1 slice2
  = step (S.singleton (fst (last slice1), fst (last slice2))) S.empty
  where
    step :: VarPairSet -> VarPairSet -> Bool
    step wanted done
        | wanted `S.isSubsetOf` done
        = True -- done
        | (x,y) : _ <- S.toList (wanted `S.difference` done)
        , (Just _, wanted') <- runState (runMaybeT (equate x y)) wanted
        = step wanted' (S.insert (x,y) done)
        | otherwise
        = False


    equate :: Var -> Var -> MaybeT (State VarPairSet) ()
    equate x y
        | it
        , Just e1 <- lookup x slice1
        , Just x' <- essentiallyVar e1
        = equated x' y
        | it
        , Just e2 <- lookup y slice2
        , Just y' <- essentiallyVar e2
        = equated x y'
        | Just e1 <- lookup x slice1
        , Just e2 <- lookup y slice2
        = go (mkRnEnv2 emptyInScopeSet) e1 e2
    equate _ _ = mzero

    equated :: Var -> Var -> MaybeT (State VarPairSet) ()
    equated x y | x == y = return ()
    equated x y = lift $ modify (S.insert (x,y))

    essentiallyVar :: CoreExpr -> Maybe Var
    essentiallyVar (App e a) | isTyCoArg a = essentiallyVar e
    essentiallyVar (Lam v e) | isTyCoVar v = essentiallyVar e
    essentiallyVar (Cast e _)              = essentiallyVar e
    essentiallyVar (Var v)                 = Just v
    essentiallyVar _                       = Nothing

    go :: RnEnv2 -> CoreExpr -> CoreExpr -> MaybeT (State (S.Set (Var,Var))) ()
    go env (Var v1) (Var v2) | rnOccL env v1 == rnOccR env v2 = pure ()
                             | otherwise = equated v1 v2
    go _   (Lit lit1)    (Lit lit2)        = guard $ lit1 == lit2
    go env (Type t1)     (Type t2)         = guard $ eqTypeX env t1 t2
    go env (Coercion co1) (Coercion co2)   = guard $ eqCoercionX env co1 co2

    go env (Cast e1 _) e2 | it             = go env e1 e2
    go env e1 (Cast e2 _) | it             = go env e1 e2
    go env (Cast e1 co1) (Cast e2 co2)     = do guard (eqCoercionX env co1 co2)
                                                go env e1 e2

    go env (App e1 a) e2 | it, isTyCoArg a = go env e1 e2
    go env e1 (App e2 a) | it, isTyCoArg a = go env e1 e2
    go env (App f1 a1)   (App f2 a2)       = go env f1 f2 >> go env a1 a2
    go env (Tick n1 e1)  (Tick n2 e2)      = guard (go_tick env n1 n2) >> go env e1 e2

    go env (Lam b e1) e2 | it, isTyCoVar b = go env e1 e2
    go env e1 (Lam b e2) | it, isTyCoVar b = go env e1 e2
    go env (Lam b1 e1)  (Lam b2 e2)
      = do guard (it || eqTypeX env (varType b1) (varType b2))
                -- False for Id/TyVar combination
           go (rnBndr2 env b1 b2) e1 e2

    go env (Let (NonRec v1 r1) e1) (Let (NonRec v2 r2) e2)
      = do go env r1 r2  -- No need to check binder types, since RHSs match
           go (rnBndr2 env v1 v2) e1 e2

    go env (Let (Rec ps1) e1) (Let (Rec ps2) e2)
      = do guard $ equalLength ps1 ps2
           sequence_ $ zipWith (go env') rs1 rs2
           go env' e1 e2
      where
        (bs1,rs1) = unzip ps1
        (bs2,rs2) = unzip ps2
        env' = rnBndrs2 env bs1 bs2

    go env (Case e1 b1 t1 a1) (Case e2 b2 t2 a2)
      | null a1   -- See Note [Empty case alternatives] in TrieMap
      = do guard (null a2)
           go env e1 e2
           guard (it || eqTypeX env t1 t2)
      | otherwise
      = do guard $ equalLength a1 a2
           go env e1 e2
           sequence_ $ zipWith (go_alt (rnBndr2 env b1 b2)) a1 a2

    go _ _ _ = guard False

    -----------
    go_alt env (c1, bs1, e1) (c2, bs2, e2)
      = guard (c1 == c2) >> go (rnBndrs2 env bs1 bs2) e1 e2

    go_tick :: RnEnv2 -> Tickish Id -> Tickish Id -> Bool
    go_tick env (Breakpoint lid lids) (Breakpoint rid rids)
          = lid == rid  &&  map (rnOccL env) lids == map (rnOccR env) rids
    go_tick _ l r = l == r



-- | Returns @True@ if the given core expression mentions no type constructor
-- anywhere that has the given name.
freeOfType :: Slice -> Name -> Maybe (Var, CoreExpr)
freeOfType slice tcN = listToMaybe [ (v,e) | (v,e) <- slice, not (go e) ]
  where
    goV v = goT (varType v)

    go (Var v)           = goV v
    go (Lit _ )          = True
    go (App e a)         = go e && go a
    go (Lam b e)         = goV b && go e
    go (Let bind body)   = all goB (flattenBinds [bind]) && go body
    go (Case s b _ alts) = go s && goV b && all goA alts
    go (Cast e _)        = go e
    go (Tick _ e)        = go e
    go (Type t)          = (goT t)
    go (Coercion _)      = True

    goB (b, e) = goV b && go e

    goA (_,pats, e) = all goV pats && go e

    goT (TyVarTy _)      = True
    goT (AppTy t1 t2)    = goT t1 && goT t2
    goT (TyConApp tc ts) = getName tc /= tcN && all goT ts
                        -- ↑ This is the crucial bit
    goT (ForAllTy _ t)   = goT t
#if MIN_VERSION_GLASGOW_HASKELL(8,2,0,0)
    goT (FunTy t1 t2)    = goT t1 && goT t2
#endif
    goT (LitTy _)        = True
    goT (CastTy t _)     = goT t
    goT (CoercionTy _)   = True

-- | True if the given variable binding does not allocate, if called fully
-- satisfied.
--
-- It currently does not look through function calls, which of course could
-- allocate. It should probably at least look through local function calls.
--
-- The variable is important to know the arity of the function.
doesNotAllocate :: Slice -> Maybe (Var, CoreExpr)
doesNotAllocate slice = listToMaybe [ (v,e) | (v,e) <- slice, not (go (idArity v) e) ]
  where
    go _ (Var v)
      | isDataConWorkId v, idArity v > 0 = False
    go a (Var v)                         = (a >= idArity v)
    go _ (Lit _ )                        = True
    go a (App e arg) | isTypeArg arg     = go a e
    go a (App e arg)                     = go (a+1) e && goArg arg
    go a (Lam b e) | isTyVar b           = go a e
    go 0 (Lam _ _)                       = False
    go a (Lam _ e)                       = go (a-1) e
    go a (Let bind body)                 = all goB (flattenBinds [bind]) && go a body
    go a (Case s _ _ alts)               = go 0 s && all (goA a) alts
    go a (Cast e _)                      = go a e
    go a (Tick _ e)                      = go a e
    go _ (Type _)                        = True
    go _ (Coercion _)                    = True

    goArg e | exprIsTrivial e             = go 0 e
            | isUnliftedType (exprType e) = go 0 e
            | otherwise                   = False

    goB (b, e)
#if MIN_VERSION_GLASGOW_HASKELL(8,2,0,0)
        | isJoinId b                = go (idArity b) e
#endif
        -- Not sure when a local function definition allocates…
        | isFunTy (idType b)        = go (idArity b) e
        | isUnliftedType (idType b) = go (idArity b) e
        | otherwise                 = False
        -- A let binding allocates if any variable is not a join point and not
        -- unlifted

    goA a (_,_, e) = go a e
