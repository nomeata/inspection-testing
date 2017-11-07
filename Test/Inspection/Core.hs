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

-- | Returns @True@ if the given core expression mentions no type constructor
-- anywhere that has the given name.
freeOfType :: Name -> CoreExpr -> Bool
freeOfType tcN = go
  where
    go (Var v)           = goV v
    go (Lit _ )          = True
    go (App e a)         = go e && go a
    go (Lam b e)         = goV b && go e
    go (Let bind body)   = all goB (flattenBinds [bind]) && go body
    go (Case s b _ alts) = go s && goV b && all goA alts
    go (Cast e _)        = go e
    go (Tick _ e)        = go e
    go (Type t)          = goT t
    go (Coercion _)      = True

    goB (b, e) = goV b && go e

    goV v = goT (varType v)

    goA (_,pats, e) = all goV pats && go e

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
doesNotAllocate :: Var -> CoreExpr -> Bool
doesNotAllocate v e = go (idArity v) e
  where
    go a (Var v)                     = a >= idArity v
    go _ (Lit _ )                    = True
    go a (App e arg) | isTypeArg arg = go a e
    go a (App e arg)                 = go (a+1) e && goArg arg
    go a (Lam b e) | isTyVar b       = go a e
    go 0 (Lam _ _)                   = False
    go a (Lam _ e)                   = go (a-1) e
    go a (Let bind body)             = all goB (flattenBinds [bind]) && go a body
    go a (Case s _ _ alts)           = go 0 s && all (goA a) alts
    go a (Cast e _)                  = go a e
    go a (Tick _ e)                  = go a e
    go _ (Type _)                    = True
    go _ (Coercion _)                = True

    goArg e = (exprIsTrivial e || isUnliftedType (exprType e)) && go 0 e

    goB (b, e) = (isJoinId b || isUnliftedType (idType b)) && go (idArity b) e
        -- A let binding allocates if any variable is not a join point and not
        -- unlifted

    goA a (_,_, e) = go a e
