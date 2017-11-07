-- | This module implements some of analyses of Core expressions necessary for
-- "Test.Inspection". Normally, users of this pacakge can ignore this module. 
module Test.Inspection.Core
  ( freeOfType
  ) where

import CoreSyn
import TyCoRep
import Var
import Name

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
