-- | This module implements some of analyses of Core expressions necessary for
-- "Test.Inspection". Normally, users of this pacakge can ignore this module. 
module Test.Inspection.Core
  ( freeOfType
  , doesNotAllocate
  ) where

import CoreSyn
import CoreUtils
import TyCoRep
import Type
import Var
import Id
import Name

import qualified Data.Set as S
import State

-- | Returns @True@ if the given core expression mentions no type constructor
-- anywhere that has the given name.
freeOfType :: [(Var, CoreExpr)] -> Name -> CoreExpr -> Bool
freeOfType binds tcN e = runSeenM $ go e
  where
    local = S.fromList (map fst binds)
    goV v | v `S.member` local = do
        whenUnseen v $ do
            let Just e = lookup v binds
            go  e
          | otherwise = goV' v

    goV' v = pure $ goT (varType v)

    go (Var v)           = goV v
    go (Lit _ )          = pure True
    go (App e a)         = go e <&&> go a
    go (Lam b e)         = goV' b <&&> go e
    go (Let bind body)   = allM goB (flattenBinds [bind]) <&&> go body
    go (Case s b _ alts) = go s <&&> goV' b <&&> allM goA alts
    go (Cast e _)        = go e
    go (Tick _ e)        = go e
    go (Type t)          = pure (goT t)
    go (Coercion _)      = pure True

    goB (b, e) = goV' b <&&> go e

    goA (_,pats, e) = allM goV pats <&&> go e

    goT (TyVarTy _)      = True
    goT (AppTy t1 t2)    = goT t1 && goT t2
    goT (TyConApp tc ts) = getName tc /= tcN && all goT ts
                        -- ↑ This is the crucial bit
    goT (ForAllTy _ t)   = goT t
    goT (FunTy t1 t2)    = goT t1 && goT t2
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
doesNotAllocate :: [(Var, CoreExpr)] -> Var -> CoreExpr -> Bool
doesNotAllocate binds v e = runSeenM $ go (idArity v) e
  where
    local = S.fromList (map fst binds)
    goV v | v `S.member` local = do
        whenUnseen v $ do
            let Just e = lookup v binds
            go (idArity v) e
          | otherwise = pure True

    go a (Var v)                     = pure (a >= idArity v) <&&> goV v
    go _ (Lit _ )                    = pure True
    go a (App e arg) | isTypeArg arg = go a e
    go a (App e arg)                 = go (a+1) e <&&> goArg arg
    go a (Lam b e) | isTyVar b       = go a e
    go 0 (Lam _ _)                   = pure False
    go a (Lam _ e)                   = go (a-1) e
    go a (Let bind body)             = allM goB (flattenBinds [bind]) <&&> go a body
    go a (Case s _ _ alts)           = go 0 s <&&> allM (goA a) alts
    go a (Cast e _)                  = go a e
    go a (Tick _ e)                  = go a e
    go _ (Type _)                    = pure True
    go _ (Coercion _)                = pure True

    goArg e | exprIsTrivial e             = go 0 e
            | isUnliftedType (exprType e) = go 0 e
            | otherwise                   = pure False

    goB (b, e) | isJoinId b                = go (idArity b) e
               | isUnliftedType (idType b) = go (idArity b) e
               | otherwise                 = pure False
        -- A let binding allocates if any variable is not a join point and not
        -- unlifted

    goA a (_,_, e) = go a e


type SeenM = State (S.Set Var)

runSeenM :: SeenM x -> x
runSeenM = flip evalState S.empty

whenUnseen :: Var -> SeenM Bool -> SeenM Bool
whenUnseen v act = do
    seen <- gets (v `S.member`)
    if seen then return True else do
        modify (S.insert v)
        act

(<&&>) :: Monad m => m Bool -> m Bool -> m Bool
m1 <&&> m2 = do { x <- m1; if x then m2 else pure False }

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM f   = foldr (<&&>) (pure True) . map f
