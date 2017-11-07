-- | This module implements some of analyses of Core expressions necessary for
-- "Test.Inspection". Normally, users of this pacakge can ignore this module. 
{-# LANGUAGE CPP #-}
module Test.Inspection.Core
  ( slice
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
import Literal (nullAddrLit)

import qualified Data.Set as S
import Data.Maybe
import State
import Control.Monad

-- | Selects those bindings that define the given variable
slice :: [(Var, CoreExpr)] -> Var -> [(Var,CoreExpr)]
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
    go (App e arg) | isTypeArg arg = go e
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

-- | This is a heuristic, which only works if both slices
-- have auxillary variables in the right order.
-- (This is mostly to work-around the buggy CSE in GHC-8.0)
-- It also breaks if there is shadowing.
eqSlice :: [(Var, CoreExpr)] -> [(Var, CoreExpr)] -> Bool
eqSlice slice1 slice2 =
    eqExpr emptyInScopeSet (Let (Rec slice1) (Lit nullAddrLit))
                           (Let (Rec slice2) (Lit nullAddrLit))

-- | Returns @True@ if the given core expression mentions no type constructor
-- anywhere that has the given name.
freeOfType :: [(Var, CoreExpr)] -> Name -> Maybe (Var, CoreExpr)
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
doesNotAllocate :: [(Var, CoreExpr)] -> Maybe (Var, CoreExpr)
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
