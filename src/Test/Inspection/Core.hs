-- | This module implements some analyses of Core expressions necessary for
-- "Test.Inspection". Normally, users of this package can ignore this module.
{-# LANGUAGE CPP, FlexibleContexts, PatternSynonyms, MultiWayIf, ViewPatterns #-}
module Test.Inspection.Core
  ( slice
  , pprSlice
  , pprSliceDifference
  , eqSlice
  , eqSlice'
  , freeOfType
  , freeOfTerm
  , doesNotAllocate
  , doesNotContainTypeClasses
  ) where

#if MIN_VERSION_ghc(9,0,0)
import GHC.Builtin.Types (isCTupleTyConName)
import GHC.Core
import GHC.Core.Utils
import GHC.Core.TyCo.Rep
import GHC.Core.Type
import GHC.Types.Var as Var
import GHC.Types.Var.Set (mkVarSet)
import GHC.Types.Id
import GHC.Types.Name
import GHC.Types.Literal
import GHC.Types.Var.Env
import GHC.Types.Unique
import GHC.Utils.Outputable as Outputable
import GHC.Core.Ppr
import GHC.Core.Subst
import GHC.Core.Coercion
import GHC.Utils.Misc
import GHC.Core.DataCon
import GHC.Core.TyCon (TyCon, isClassTyCon)
#else
import TysWiredIn (isCTupleTyConName)
import CoreSyn
import CoreUtils
import CoreSubst
import TyCoRep
import Type
import Var
import VarSet (mkVarSet)
import Id
import Literal
import Name
import VarEnv
import Outputable
import PprCore
import Coercion
import Util
import DataCon
import Unique
import TyCon (TyCon, isClassTyCon)
#endif

#if MIN_VERSION_ghc(9,2,0)
import GHC.Types.Tickish
#endif

#if MIN_VERSION_ghc(9,6,0)
import GHC.Core.TyCo.Compare (eqTypeX)
#endif

import qualified Data.Set as S
import Control.Monad (unless)
import Control.Monad.State.Strict (execState,  modify', gets)
import Data.List (nub, intercalate)
import Data.Maybe (listToMaybe, fromJust)
import Data.Either (isRight)

import Test.Inspection (Equivalence (..))

-- Uncomment to enable debug traces
-- #define DEBUG_TRACE

#ifdef DEBUG_TRACE
import Debug.Trace

tracePut :: Monad m => Int -> String -> String -> m ()
tracePut lv name msg = traceM $ replicate lv ' ' ++ name ++ ": " ++ msg
#else
tracePut :: Monad m => Int -> String -> String -> m ()
tracePut _  _    _ = return ()
#endif

#if !MIN_VERSION_ghc(9,2,0)
pattern Alt :: a -> b -> c -> (a, b, c)
pattern Alt a b c = (a, b, c)
{-# COMPLETE Alt #-}
#endif


type Slice = [(Var, CoreExpr)]

-- | Selects those bindings that define the given variable (with this variable first)
slice :: [(Var, CoreExpr)] -> Var -> Slice
slice binds v
    | Just e <- lookup v binds
    = (v,e) : [(v',e) | (v',e) <- binds, v' /= v, v' `S.member` used ]
    | otherwise
    = error "slice: cannot find given variable in bindings"
  where
    used = execState (goV v) S.empty

    local = S.fromList (map fst binds)
    goV v | v `S.member` local = do
        seen <- gets (v `S.member`)
        unless seen $ do
            modify' (S.insert v)
            let e = fromJust $ lookup v binds
            go e
          | otherwise = return ()

    go (Var v)                     = goV v
    go (Lit _ )                    = pure ()
    go (App e arg) | isTyCoArg arg = go e
    go (App e arg)                 = go e >> go arg
    go (Lam _ e)                   = go e
    go (Let bind body)             = mapM_ go (rhssOfBind bind) >> go body
    go (Case s _ _ alts)           = go s >> mapM_ goA alts
    go (Cast e _)                  = go e
    go (Tick _ e)                  = go e
    go (Type _)                    = pure ()
    go (Coercion _)                = pure ()

    goA (Alt _ _ e) = go e

-- | Pretty-print a slice
pprSlice :: Slice -> SDoc
pprSlice slice =
    withLessDetail $ pprCoreBindings [ NonRec v e  | (v,e) <- slice ]

-- | Pretty-print two slices, after removing variables occurring in both
pprSliceDifference :: Slice -> Slice -> SDoc
pprSliceDifference slice1 slice2
    | [(v1,e1)] <- slice1'
    , [(v2,e2)] <- slice2'
    = pprSingletonSliceDifference (mkInScopeSet (mkVarSet (S.toList both))) v1 v2 e1 e2

    | otherwise =
        hang (text "LHS" Outputable.<> colon) 4 (pprSlice slice1') $$
        hang (text "RHS" Outputable.<> colon) 4 (pprSlice slice2')
  where
    both = S.intersection (S.fromList (map fst slice1)) (S.fromList (map fst slice2))
    slice1' = filter (\(v,_) -> v `S.notMember` both) slice1
    slice2' = filter (\(v,_) -> v `S.notMember` both) slice2

pprSingletonSliceDifference :: InScopeSet -> Var -> Var -> CoreExpr -> CoreExpr -> SDoc
pprSingletonSliceDifference iss v1 v2 e1 e2 =
    ctxDoc $
    hang (text "LHS" Outputable.<> colon) 4 (hang (pprPrefixOcc v1) 2 (eqSign <+> pprCoreExpr e1')) $$
    hang (text "RHS" Outputable.<> colon) 4 (hang (pprPrefixOcc v2) 2 (eqSign <+> pprCoreExpr e2'))
  where
    hasContext = not (null ctxt)

    ctxDoc | hasContext = id
           | otherwise = (hang (text "In") 4 (ppr $ mkContextExpr (reverse (map snd ctxt))) $$)

    eqSign | hasContext = text "= ..."
           | otherwise  = equals

    (e1', e2', ctxt) = go e1 e2 [] (mkRnEnv2 emptyInScopeSet)

    go :: CoreExpr -> CoreExpr -> [(Var, Var)] -> RnEnv2 -> (CoreExpr, CoreExpr, [(Var, Var)])
    go (Lam b1 t1) (Lam b2 t2) ctxt env
        | eqTypeX env (varType b1) (varType b2)
        = go t1 t2 ((b1,b2):ctxt) (rnBndr2 env b1 b2)
      where
    go x y ctxt _env = (rename iss ctxt x, y, ctxt)

    mkContextExpr :: [Var] -> CoreExpr
    mkContextExpr []       = ellipsis
    mkContextExpr (x:rest) = Lam x (mkContextExpr rest)

    ellipsis :: CoreExpr
#if MIN_VERSION_ghc(8,8,0)
    ellipsis = Lit $ mkLitString "..."
#else
    ellipsis = Lit $ mkMachString "..."
#endif

withLessDetail :: SDoc -> SDoc
#if MIN_VERSION_GLASGOW_HASKELL(8,2,0,0) && !MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
withLessDetail sdoc = sdocWithDynFlags $ \dflags ->
     withPprStyle (defaultUserStyle dflags) sdoc
#else
withLessDetail sdoc = withPprStyle defaultUserStyle sdoc
#endif

-- | Equality name environment.
-- This environment contains top-level and let-bound definitions
-- (no lambda arguments).
data EqEnv = EqEnv !Int [(Var, (Int, CoreExpr))]  [(Var, (Int, CoreExpr))]

-- | Lookup left and right variables in EqEnv.
lookupEqEnv :: Var -> Var -> EqEnv -> Maybe (Int, Int, CoreExpr, CoreExpr)
lookupEqEnv x y (EqEnv _ env1 env2)
    | Just (i, e1) <- lookup x env1
    , Just (j, e2) <- lookup y env2
    = Just (i, j, e1, e2)

    | otherwise
    = Nothing

initialEqEnv :: [(Var, CoreExpr)] -> [(Var, CoreExpr)] -> EqEnv
initialEqEnv ls rs = EqEnv 1
    [ (x, (0, e1)) | (x, e1) <- ls ]
    [ (y, (0, e2)) | (y, e2) <- rs ]

bindEqEnv :: [(Var, CoreExpr)] -> [(Var, CoreExpr)] -> EqEnv -> EqEnv
bindEqEnv ls rs (EqEnv i env1 env2)= EqEnv (i + 1)
    ([ (x, (i, e1)) | (x, e1) <- ls ] ++ env1)
    ([ (y, (i, e2)) | (y, e2) <- rs ] ++ env2)

-- | This is a heuristic, which only works if both slices
-- have auxiliary variables in the right order.
-- (This is mostly to work-around the buggy CSE in GHC-8.0)
-- It also breaks if there is shadowing.
eqSlice :: Equivalence -> Slice -> Slice -> Bool
eqSlice eqv l r = isRight (eqSlice' eqv l r)

-- | Like 'eqSlice' but also returns an explanation of inequality.
eqSlice' :: Equivalence -> Slice -> Slice -> Either SDoc ()
eqSlice' _ [] [] = Right ()
eqSlice' _ ((v,_) : _) [] = Left $ ppr v
eqSlice' _ [] ((v,_) : _) = Left $ ppr v
  -- Mostly defensive programming (slices should not be empty)
eqSlice' eqv slice1@((head1, def1) : _) slice2@((head2, def2) : _) = do
    goStart head1 def1 head2 def2
  where
    -- ignore types and hpc ticks
    it :: Bool
    it = case eqv of
        StrictEquiv              -> False
        IgnoreTypesAndTicksEquiv -> True
        UnorderedLetsEquiv       -> True

    -- unordered lets
    ul :: Bool
    ul = case eqv of
        StrictEquiv              -> False
        IgnoreTypesAndTicksEquiv -> False
        UnorderedLetsEquiv       -> True

    goStart :: Var -> CoreExpr -> Var -> CoreExpr -> Either SDoc ()
    goStart x e1 y e2 = do
        tracePut 0 "TOP" (varToString x ++ " =?= " ++ varToString y)
        if | Just x'  <- essentiallyVar e1
           , Just e1' <- lookup x' slice1
           -> goStart x' e1' y e2

           | Just y'  <- essentiallyVar e2
           , Just e2' <- lookup y' slice2
           -> goStart x e1 y' e2'

            -- otherwise if neither x and y expressions are variables
            -- 1. compare the expressions (already assuming that x and y are equal)
            -- 2. comparison may create new obligations, loop.
           | otherwise
           -> do
              let env = rnBndr2 (mkRnEnv2 emptyInScopeSet) x y
                  ee  = initialEqEnv slice1 slice2
              go [] 0 env ee e1 e2

    essentiallyVar :: CoreExpr -> Maybe Var
    essentiallyVar (App e a)  | it, isTyCoArg a = essentiallyVar e
    essentiallyVar (Lam v e)  | it, isTyCoVar v = essentiallyVar e
    essentiallyVar (Cast e _) | it              = essentiallyVar e
#if MIN_VERSION_ghc(9,0,0)
    essentiallyVar (Case s b _ alts) | it, Just e <- isUnsafeEqualityCase s b alts = essentiallyVar e
#endif
    essentiallyVar (Var v)                      = Just v
    essentiallyVar (Tick HpcTick{} e) | it      = essentiallyVar e
    essentiallyVar (Tick SourceNote{} e)        = essentiallyVar e
    essentiallyVar _                            = Nothing

    -- report inequality
    inequality :: [SDoc] -> SDoc -> Either SDoc a
    inequality []  err = Left err
    inequality ctx err = Left $ err $$ hang (text "in") 2 (vcat ctx)

    go :: [SDoc] -> Int -> RnEnv2 -> EqEnv -> CoreExpr -> CoreExpr -> Either SDoc ()
    go ctx lv env ee (essentiallyVar -> Just v1) (essentiallyVar -> Just v2) = do
        -- NOTE: The ordering of this checks is important.
        --
        -- See example https://github.com/nomeata/inspection-testing/pull/89#issuecomment-2967774956
        -- GHC may use same name for local names in different bindings.
        --
        -- Therefore we first check for already visited and lambda-bound variables.
        -- (inRnEnvL/inRnEnvR checks are important so we don't confuse for unbound variables).
        -- We need to this first to break loops of recursive definitions,.
        --
        -- Then we test whether variables are top or local-let bound.
        -- If so, we proceed to compare their definitions.
        --
        -- Lastly we check whether variables are equal.
        -- This case checks for any global names.
        if
           | inRnEnvL env v1
           , inRnEnvR env v2
           , rnOccL env v1 == rnOccR env v2 -> do
            tracePut lv "VAR" (varToString v1 ++ " =?= " ++ varToString v2 ++ " IN ENV")
            return ()

           | Just (i, j, e1, e2) <- lookupEqEnv v1 v2 ee ->
             -- we check that levels are the same
             -- if they are not we fail immediately and not fall-back further.
             if i == j
             then do
               tracePut lv "VAR" (varToString v1 ++ " =?= " ++ varToString v2 ++ " BOUND " ++ show i)
               let ctx' = text "comparing definitions of" <+> ppr v1 <+> text "=?=" <+> ppr v2 : ctx
               let env' = rnBndr2 env v1 v2
               go ctx' lv env' ee e1 e2
             else do
               tracePut lv "VAR" (varToString v1 ++ " =?= " ++ varToString v2 ++ " BOUND IN DIFFERENT LETS " ++ show i ++ " /= " ++ show j)
               inequality ctx $ hsep [ text "variables", ppr v1, text "and", ppr v2, text "are bound in different lets" ]

           | v1 == v2 -> do
            tracePut lv "VAR" (varToString v1 ++ " =?= " ++ varToString v2 ++ " SAME")
            return ()

           | otherwise -> do
            tracePut lv "VAR" (varToString v1 ++ " =?= " ++ varToString v2 ++ " NOT EQUAL")
            inequality ctx $ hsep [ text "inequal variables", ppr v1, text "and", ppr v2 ]

    go ctx lv _   _  (Lit lit1)    (Lit lit2)        = do
        tracePut lv "LIT" "???" -- no Show for Literal :(
        unless (lit1 == lit2) $ inequality ctx $ sep [ text "inequal literals", ppr lit1, text "and", ppr lit2 ]

    go ctx _  env _  (Type t1)     (Type t2)         =
        goTypes ctx env t1 t2

    go ctx _  env _  (Coercion co1) (Coercion co2)   =
        goCoercions ctx env co1 co2

    go ctx lv env ee (Cast e1 _) e2 | it             = go ctx lv env ee e1 e2
    go ctx lv env ee e1 (Cast e2 _) | it             = go ctx lv env ee e1 e2
#if MIN_VERSION_ghc(9,0,0)
    go ctx lv env ee (Case s b _ alts) e2 | it, Just e1 <- isUnsafeEqualityCase s b alts = go ctx lv env ee e1 e2
    go ctx lv env ee e1 (Case s b _ alts) | it, Just e2 <- isUnsafeEqualityCase s b alts = go ctx lv env ee e1 e2
#endif
    go ctx lv env ee (Cast e1 co1) (Cast e2 co2)     = traceBlock lv "CAST" "" $ \lv -> do
                                                   goCoercions ctx env co1 co2
                                                   go ctx lv env ee e1 e2

    go ctx lv env ee (App e1 a) e2 | it, isTyCoArg a = go ctx lv env ee e1 e2
    go ctx lv env ee e1 (App e2 a) | it, isTyCoArg a = go ctx lv env ee e1 e2
    go ctx lv env ee (App f1 a1)   (App f2 a2)       = traceBlock lv "APP" "" $ \lv -> do
                                                   go ctx lv env ee f1 f2
                                                   go ctx lv env ee a1 a2
    go ctx lv env ee (Tick HpcTick{} e1) e2 | it     = go ctx lv env ee e1 e2
    go ctx lv env ee e1 (Tick HpcTick{} e2) | it     = go ctx lv env ee e1 e2
    go ctx lv env ee (Tick SourceNote{} e1) e2       = go ctx lv env ee e1 e2
    go ctx lv env ee e1 (Tick SourceNote{} e2)       = go ctx lv env ee e1 e2
    go ctx lv env ee (Tick n1 e1)  (Tick n2 e2)      = traceBlock lv "TICK" "" $ \lv -> do
                                                   unless (go_tick env n1 n2) $ inequality ctx $ text "inequal ticks"
                                                   go ctx lv env ee e1 e2

    go ctx lv env ee (Lam b e1) e2 | it, isTyCoVar b = go ctx lv env ee e1 e2
    go ctx lv env ee e1 (Lam b e2) | it, isTyCoVar b = go ctx lv env ee e1 e2
    go ctx lv env ee (Lam b1 e1)  (Lam b2 e2)        = traceBlock lv "LAM" (varToString b1 ++ " ~ " ++ varToString b2) $ \lv -> do
           -- guard (it || eqTypeX env (varType b1) (varType b2)) -- TODO
           go ctx lv (rnBndr2 env b1 b2) ee e1 e2

    go ctx lv env ee e1@(Let _ _) e2@(Let _ _)
      | ul
      , (ps1, e1') <- peelLets e1
      , (ps2, e2') <- peelLets e2
      = traceBlock lv "LET" (showVars ps1 ++ " ~ " ++ showVars ps2) $ \lv -> do
           let ctx' = text "let bindings:" <+> pprVars ps1 <+> pprVars ps2 : ctx
           unless (equalLength ps1 ps2) $ inequality ctx $ text "different amount of bindings in let"
           let ee' = bindEqEnv ps1 ps2 ee
           go ctx' lv env ee' e1' e2'

    go ctx lv env ee (Let (NonRec v1 r1) e1) (Let (NonRec v2 r2) e2)
      = do go ctx lv env ee r1 r2  -- No need to check binder types, since RHSs match
           go ctx lv (rnBndr2 env v1 v2) ee e1 e2

    go ctx lv env ee (Let (Rec ps1) e1) (Let (Rec ps2) e2)
      = do
           unless (equalLength ps1 ps2) $ inequality ctx $ text "different amount of bindings in recursive let"
           sequence_ $ zipWith (go ctx lv env' ee) rs1 rs2
           go ctx lv env' ee e1 e2
      where
        bs1, bs2 :: [CoreBndr]
        rs1, rs2 :: [CoreExpr]

        (bs1,rs1) = unzip ps1
        (bs2,rs2) = unzip ps2
        env' = rnBndrs2 env bs1 bs2

    go ctx lv env ee (Case e1 b1 t1 a1) (Case e2 b2 t2 a2)
      | null a1   -- See Note [Empty case alternatives] in TrieMap
      , null a2
      = do
           go ctx lv env ee e1 e2
           unless it $ goTypes ctx env t1 t2

      | otherwise
      = traceBlock lv "CASE" "..." $ \lv -> do
           unless (equalLength a1 a2) $ inequality ctx $ text "different amount of alternatives in case"
           go ctx lv env ee e1 e2
           sequence_ $ zipWith (go_alt ctx lv (rnBndr2 env b1 b2) ee) a1 a2

    go ctx lv _ _  e1 e2 = do
        tracePut lv "FAIL" (conToString e1 ++ " =/= " ++ conToString e2)
        inequality ctx $ sep [ text "inequal terms:", ppr e1, text "and", ppr e2]

    goCoercions ctx env t1 t2
        | eqCoercionX env t1 t2 = return ()
        | otherwise             = inequality ctx $ sep [ text "inequal coercions:", ppr t1, text "and", ppr t2 ]

    goTypes ctx env t1 t2
        | eqTypeX env t1 t2 = return ()
        | otherwise         = inequality ctx $ sep [ text "inequal types:", ppr t1, text "and", ppr t2 ]

    -----------
    go_alt :: [SDoc] -> Int -> RnEnv2 -> EqEnv -> CoreAlt -> CoreAlt -> Either SDoc ()
    go_alt ctx lv env ee (Alt c1 bs1 e1) (Alt c2 bs2 e2)
      = traceBlock lv "ALT" "..." $ \lv -> do
           unless (c1 == c2) $ inequality ctx $ sep [ text "inequal constructors:", ppr c1, text "and", ppr c2 ]
           go ctx lv (rnBndrs2 env bs1 bs2) ee e1 e2

    go_tick :: RnEnv2 -> CoreTickish -> CoreTickish -> Bool
    go_tick env Breakpoint{ breakpointId = lid, breakpointFVs = lids } Breakpoint{ breakpointId = rid, breakpointFVs = rids }
          = lid == rid  &&  map (rnOccL env) lids == map (rnOccR env) rids
    go_tick _ l r = l == r

    peelLets (Let (NonRec v r) e) = let (xs, e') = peelLets e in ((v,r):xs, e')
    peelLets (Let (Rec bs) e)     = let (xs, e') = peelLets e in (bs ++ xs, e')
    peelLets e                    = ([], e)

#if !MIN_VERSION_ghc(9,9,0) && MIN_VERSION_ghc(9,0,0)
isUnsafeEqualityCase :: CoreExpr -> Id -> [CoreAlt] -> Maybe CoreExpr
isUnsafeEqualityCase scrut _bndr [Alt _ _ rhs]
  | isUnsafeEqualityProof scrut = Just rhs
isUnsafeEqualityCase _ _ _ = Nothing
#endif

#if !MIN_VERSION_ghc(9,2,0)
type CoreTickish = Tickish Id
#endif

traceBlock :: Monad m => Int -> String -> String -> (Int -> m ()) -> m ()
traceBlock lv name msg action = do
    tracePut lv name msg
    action (lv + 1)
    tracePut lv name $ msg ++ " OK"

showVars :: [(Var, a)] -> String
showVars xs = intercalate ", " [ varToString x | (x, _) <- xs ]

pprVars :: [(Var, a)] -> SDoc
pprVars = braces . hsep . map (ppr . fst)

varToString :: Var -> String
varToString v = occNameString (occName (tyVarName v)) ++ "_" ++ show (getUnique v)
-- using tyVarName as varName is ambiguous.

conToString :: CoreExpr -> [Char]
conToString Var {}      = "Var"
conToString Lit {}      = "Lit"
conToString App {}      = "App"
conToString Lam {}      = "Lam"
conToString Let {}      = "Let"
conToString Case {}     = "Case"
conToString Cast {}     = "Cast"
conToString Tick {}     = "Tick"
conToString Type {}     = "Type"
conToString Coercion {} = "Coercion"

-- | Returns @True@ if the given core expression mentions no type constructor
-- anywhere that has the given name.
freeOfType :: Slice -> [Name] -> Maybe (Var, CoreExpr)
freeOfType slice tcNs =
      fmap (\(a,b,_) -> (a,b))
    $ allTyCons (\tc -> getName tc `notElem` tcNs) slice

-- | Check if all type constructors in a slice satisfy the given predicate.
-- Returns the binder, expression and failing constructors triple on failure.
allTyCons :: (TyCon -> Bool) -> Slice -> Maybe (Var, CoreExpr, [TyCon])
allTyCons ignore slice =
    listToMaybe
        [(v, e, nub tcs) | (v, e) <- slice, let tcs = go e, not (null tcs)]
  where
    goV v = goT (varType v)

    go (Var v)           = goV v
    go (Lit _)           = []
    go (App e a)         = go e ++ go a
    go (Lam b e)         = goV b ++ go e
    go (Let bind body)   = concatMap goB (flattenBinds [bind]) ++ go body
    go (Case s b _ alts) = go s ++ goV b ++ concatMap goA alts
    go (Cast e _)        = go e
    go (Tick _ e)        = go e
    go (Type t)          = (goT t)
    go (Coercion _)      = []

    goB (b, e) = goV b ++ go e

    goA (Alt _ pats e) = concatMap goV pats ++ go e

    goT (TyVarTy _)      = []
    goT (AppTy t1 t2)    = goT t1 ++ goT t2
    goT (TyConApp tc ts) = [tc | not (ignore tc)] ++ concatMap goT ts
                           -- ↑ This is the crucial bit
    goT (ForAllTy _ t)   = goT t
#if MIN_VERSION_GLASGOW_HASKELL(8,2,0,0)
    goT (FunTy
#if MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
               _
#endif
# if MIN_VERSION_GLASGOW_HASKELL(8,9,0,0)
               _
# endif
                 t1 t2)  = goT t1 ++ goT t2
#endif
    goT (LitTy _)        = []
    goT (CastTy t _)     = goT t
    goT (CoercionTy _)   = []
--
-- | Returns @True@ if the given core expression mentions no term variable
-- anywhere that has the given name.
freeOfTerm :: Slice -> [Name] -> Maybe (Var, CoreExpr)
freeOfTerm slice needles = listToMaybe [ (v,e) | (v,e) <- slice, not (go e) ]
  where
    isNeedle n = n `elem` needles

    goV v | isNeedle (Var.varName v)  = False
          | Just dc <- isDataConId_maybe v
          , isNeedle (dataConName dc) = False
          | otherwise                 = True

    go (Var v)           = goV v
    go (Lit _ )          = True
    go (App e a)         = go e && go a
    go (Lam _ e)         = go e
    go (Let bind body)   = all goB (flattenBinds [bind]) && go body
    go (Case s _ _ alts) = go s && all goA alts
    go (Cast e _)        = go e
    go (Tick _ e)        = go e
    go (Type _)          = True
    go (Coercion _)      = True

    goB (_, e) = go e

    goA (Alt ac _ e) = goAltCon ac && go e

    goAltCon (DataAlt dc) | isNeedle (dataConName dc) = False
    goAltCon _ = True


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
    go a (Var v)                         = a >= idArity v
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

    goA a (Alt _ _ e) = go a e

doesNotContainTypeClasses :: Slice -> [Name] -> Maybe (Var, CoreExpr, [TyCon])
doesNotContainTypeClasses slice tcNs
    = allTyCons (\tc -> not (isClassTyCon tc) || isCTupleTyConName (getName tc) || any (getName tc ==) tcNs) slice

rename :: InScopeSet -> [(Var, Var)] -> CoreExpr -> CoreExpr
rename iss rn = substExpr' sub where
    -- convert RnEnv2 to Subst
    -- here we forget about tyvars and covars, but mostly this is good enough.
    sub = mkOpenSubst iss [ (v1, if isTyVar v2 then Type (mkTyVarTy v2) else if isCoVar v2 then Coercion (mkCoVarCo v2) else Var v2 ) | (v1, v2) <- rn]

#if MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
    substExpr' = substExpr
#else
    substExpr' = substExpr empty
#endif
