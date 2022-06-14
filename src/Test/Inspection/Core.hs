-- | This module implements some analyses of Core expressions necessary for
-- "Test.Inspection". Normally, users of this package can ignore this module.
{-# LANGUAGE CPP, FlexibleContexts, PatternSynonyms, MultiWayIf #-}
module Test.Inspection.Core
  ( slice
  , pprSlice
  , pprSliceDifference
  , eqSlice
  , freeOfType
  , freeOfTerm
  , doesNotAllocate
  , doesNotContainTypeClasses
  ) where

#if MIN_VERSION_ghc(9,0,0)
import GHC.Core
import GHC.Core.Utils
import GHC.Core.TyCo.Rep
import GHC.Core.Type
import GHC.Types.Var as Var
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
import CoreSyn
import CoreUtils
import CoreSubst
import TyCoRep
import Type
import Var
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

import qualified Data.Set as S
import Control.Monad.State.Strict
import Data.List (nub, intercalate)
import Data.Maybe

import Test.Inspection (Equivalence (..))

-- Uncomment to enable debug traces
-- import Debug.Trace

tracePut :: Monad m => Int -> String -> String -> m ()
-- tracePut lv name msg = traceM $ replicate lv ' ' ++ name ++ ": " ++ msg
tracePut _  _    _ = return ()

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
            modify (S.insert v)
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
    = pprSingletonSliceDifference v1 v2 e1 e2

    | otherwise =
        hang (text "LHS" Outputable.<> colon) 4 (pprSlice slice1') $$
        hang (text "RHS" Outputable.<> colon) 4 (pprSlice slice2')
  where
    both = S.intersection (S.fromList (map fst slice1)) (S.fromList (map fst slice2))
    slice1' = filter (\(v,_) -> v `S.notMember` both) slice1
    slice2' = filter (\(v,_) -> v `S.notMember` both) slice2

pprSingletonSliceDifference :: Var -> Var -> CoreExpr -> CoreExpr -> SDoc
pprSingletonSliceDifference v1 v2 e1 e2 =
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
    go x y ctxt _env = (rename ctxt x, y, ctxt)

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

type VarPair = (Var, Var)
type VarPairSet = S.Set VarPair

-- | This is a heuristic, which only works if both slices
-- have auxiliary variables in the right order.
-- (This is mostly to work-around the buggy CSE in GHC-8.0)
-- It also breaks if there is shadowing.
eqSlice :: Equivalence -> Slice -> Slice -> Bool
eqSlice _ slice1 slice2 | null slice1 || null slice2 = null slice1 == null slice2
  -- Mostly defensive programming (slices should not be empty)
eqSlice eqv slice1 slice2
    -- slices are equal if there exist any result with no "unification" obligations left.
    = any (S.null . snd) results
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

    -- results. If there are no pairs to be equated, all is fine.
    results :: [((), VarPairSet)]
    results = runStateT (loop' (mkRnEnv2 emptyInScopeSet) S.empty (fst (head slice1)) (fst (head slice2))) S.empty

    -- while there are obligations left, try to equate them.
    loop :: RnEnv2 -> VarPairSet -> StateT VarPairSet [] ()
    loop env done = do
        vars <- get
        case S.minView vars of
            Nothing -> return () -- nothing to do, done.
            Just ((x, y), vars') -> do
                put vars'
                if (x, y) `S.member` done
                then loop env done
                else loop' env done x y

    loop' :: RnEnv2 -> VarPairSet -> Var -> Var -> StateT VarPairSet [] ()
    loop' env done x y = do
        tracePut 0 "TOP" (varToString x ++ " =?= " ++ varToString y)
        tracePut 0 "DONESET" (showVarPairSet done)

        -- if x or y expressions are essentially a variable x' or y' respectively
        -- add an obligation to check x' = y (or x = y').
        if | Just e1 <- lookup x slice1
           , Just x' <- essentiallyVar e1
           , x' `elem` map fst slice1
           -> do modify' (S.insert (x', y))
                 loop env done

           | Just e2 <- lookup y slice2
           , Just y' <- essentiallyVar e2
           , y' `elem` map fst slice2
           -> do modify' (S.insert (x, y'))
                 loop env done

            -- otherwise if neither x and y expressions are variables
            -- 1. compare the expressions (already assuming that x and y are equal)
            -- 2. comparison may create new obligations, loop.
           | Just e1 <- lookup x slice1
           , Just e2 <- lookup y slice2
           -> do
               let env' = rnBndr2 env x y
                   done' = S.insert (x, y) done

               go 0 env' e1 e2
               loop env' done'

            -- and finally, if x or y are not in the slice, we abort.
           | otherwise
           -> do
              tracePut 0 "TOP" (varToString x ++ " =?= " ++ varToString y ++ " NOT IN SLICES")
              mzero

    essentiallyVar :: CoreExpr -> Maybe Var
    essentiallyVar (App e a)  | it, isTyCoArg a = essentiallyVar e
    essentiallyVar (Lam v e)  | it, isTyCoVar v = essentiallyVar e
    essentiallyVar (Cast e _) | it              = essentiallyVar e
#if MIN_VERSION_ghc(9,0,0)
    essentiallyVar (Case s _ _ [Alt _ _ e]) | it, isUnsafeEqualityProof s = essentiallyVar e
#endif
    essentiallyVar (Var v)                      = Just v
    essentiallyVar (Tick HpcTick{} e) | it      = essentiallyVar e
    essentiallyVar _                            = Nothing

    go :: Int -> RnEnv2 -> CoreExpr -> CoreExpr -> StateT VarPairSet [] ()
    go lv env (Var v1) (Var v2) = do
        if | v1 == v2 -> do
            tracePut lv "VAR" (varToString v1 ++ " =?= " ++ varToString v2 ++ " SAME")
            return ()
           | rnOccL env v1 == rnOccR env v2 -> do
            tracePut lv "VAR" (varToString v1 ++ " =?= " ++ varToString v2 ++ " IN ENV")
            return ()
           | otherwise -> do
            tracePut lv "VAR" (varToString v1 ++ " =?= " ++ varToString v2 ++ " OBLIGATION")
            modify (S.insert (v1, v2))

    go lv _   (Lit lit1)    (Lit lit2)        = do
        tracePut lv "LIT" "???" -- no Show for Literal :(
        guard $ lit1 == lit2

    go _  env (Type t1)     (Type t2)         = guard $ eqTypeX env t1 t2
    go _  env (Coercion co1) (Coercion co2)   = guard $ eqCoercionX env co1 co2

    go lv env (Cast e1 _) e2 | it             = go lv env e1 e2
    go lv env e1 (Cast e2 _) | it             = go lv env e1 e2
#if MIN_VERSION_ghc(9,0,0)
    go lv env (Case s _ _ [Alt _ _ e1]) e2 | it, isUnsafeEqualityProof s = go lv env e1 e2
    go lv env e1 (Case s _ _ [Alt _ _ e2]) | it, isUnsafeEqualityProof s = go lv env e1 e2
#endif
    go lv env (Cast e1 co1) (Cast e2 co2)     = traceBlock lv "CAST" "" $ \lv -> do
                                                   guard (eqCoercionX env co1 co2)
                                                   go lv env e1 e2

    go lv env (App e1 a) e2 | it, isTyCoArg a = go lv env e1 e2
    go lv env e1 (App e2 a) | it, isTyCoArg a = go lv env e1 e2
    go lv env (App f1 a1)   (App f2 a2)       = traceBlock lv "APP" "" $ \lv -> do
                                                   go lv env f1 f2
                                                   go lv env a1 a2
    go lv env (Tick HpcTick{} e1) e2 | it     = go lv env e1 e2
    go lv env e1 (Tick HpcTick{} e2) | it     = go lv env e1 e2
    go lv env (Tick n1 e1)  (Tick n2 e2)      = traceBlock lv "TICK" "" $ \lv -> do
                                                   guard (go_tick env n1 n2)
                                                   go lv env e1 e2

    go lv env (Lam b e1) e2 | it, isTyCoVar b = go lv env e1 e2
    go lv env e1 (Lam b e2) | it, isTyCoVar b = go lv env e1 e2
    go lv env (Lam b1 e1)  (Lam b2 e2)        = traceBlock lv "LAM" (varToString b1 ++ " ~ " ++ varToString b2) $ \lv -> do
           guard (it || eqTypeX env (varType b1) (varType b2))
           go lv (rnBndr2 env b1 b2) e1 e2

    go lv env e1@(Let _ _) e2@(Let _ _)
      | ul
      , (ps1, e1') <- peelLets e1
      , (ps2, e2') <- peelLets e2
      = traceBlock lv "LET" (showVars ps1 ++ " ~ " ++ showVars ps2) $ \lv -> do
           guard $ equalLength ps1 ps2
           env' <- goBinds lv env ps1 ps2
           go lv env' e1' e2'

    go lv env (Let (NonRec v1 r1) e1) (Let (NonRec v2 r2) e2)
      = do go lv env r1 r2  -- No need to check binder types, since RHSs match
           go lv (rnBndr2 env v1 v2) e1 e2

    go lv env (Let (Rec ps1) e1) (Let (Rec ps2) e2)
      = do guard $ equalLength ps1 ps2
           sequence_ $ zipWith (go lv env') rs1 rs2
           go lv env' e1 e2
      where
        (bs1,rs1) = unzip ps1
        (bs2,rs2) = unzip ps2
        env' = rnBndrs2 env bs1 bs2

    go lv env (Case e1 b1 t1 a1) (Case e2 b2 t2 a2)
      | null a1   -- See Note [Empty case alternatives] in TrieMap
      = do guard (null a2)
           go lv env e1 e2
           guard (it || eqTypeX env t1 t2)
      | otherwise
      = do guard $ equalLength a1 a2
           go lv env e1 e2
           sequence_ $ zipWith (go_alt lv (rnBndr2 env b1 b2)) a1 a2

    go lv _ e1 e2 = do
        tracePut lv "FAIL" (conToString e1 ++ " =/= " ++ conToString e2)
        mzero

    -----------
    go_alt lv env (Alt c1 bs1 e1) (Alt c2 bs2 e2)
      = guard (c1 == c2) >> go lv (rnBndrs2 env bs1 bs2) e1 e2

#if MIN_VERSION_ghc(9,2,0)
    go_tick :: RnEnv2 -> CoreTickish -> CoreTickish -> Bool
    go_tick env (Breakpoint _ lid lids) (Breakpoint _ rid rids)
#else
    go_tick :: RnEnv2 -> Tickish Id -> Tickish Id -> Bool
    go_tick env (Breakpoint lid lids) (Breakpoint rid rids)
#endif
          = lid == rid  &&  map (rnOccL env) lids == map (rnOccR env) rids
    go_tick _ l r = l == r

    peelLets (Let (NonRec v r) e) = let (xs, e') = peelLets e in ((v,r):xs, e')
    peelLets (Let (Rec bs) e)     = let (xs, e') = peelLets e in (bs ++ xs, e')
    peelLets e                    = ([], e)

    goBinds :: Int -> RnEnv2 -> [(Var, CoreExpr)] -> [(Var, CoreExpr)] -> StateT VarPairSet [] RnEnv2
    goBinds _  env []           []    = return env
    goBinds _  _   []           (_:_) = mzero
    goBinds lv env ((v1,b1):xs) ys'   = do
        -- select a binding
        ((v2,b2), ys) <- lift (choices ys')

        traceBlock lv "LET*" (varToString v1 ++ " =?= " ++ varToString v2) $ \lv ->
            go lv env b1 b2

        -- if match succeeds, delete it from the obligations
        modify (S.delete (v1, v2))
        -- continue with the rest of bindings, adding a pair as matching one.
        goBinds lv (rnBndr2 env v1 v2) xs ys


traceBlock :: Monad m => Int -> String -> String -> (Int -> m ()) -> m ()
traceBlock lv name msg action = do
    tracePut lv name msg
    action (lv + 1)
    tracePut lv name $ msg ++ " OK"

showVars :: [(Var, a)] -> String
showVars xs = intercalate ", " [ varToString x | (x, _) <- xs ]

showVarPairSet :: VarPairSet -> String
showVarPairSet xs = intercalate ", " [ varToString x ++ " ~ " ++ varToString y | (x, y) <- S.toList xs ]

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

-- |
--
-- >>> choices ""
-- []
--
-- >>> choices "abcde"
-- [('a',"bcde"),('b',"acde"),('c',"abde"),('d',"abce"),('e',"abcd")]
--
choices :: [a] -> [(a, [a])]
choices = go id where
    go :: ([a] -> [a]) -> [a] -> [(a, [a])]
    go _ [] = []
    go f (x:xs) = (x, f xs) : go (f . (x :)) xs

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
    = allTyCons (\tc -> not (isClassTyCon tc) || any (getName tc ==) tcNs) slice

rename :: [(Var, Var)] -> CoreExpr -> CoreExpr
rename rn = substExpr' sub where
    -- convert RnEnv2 to Subst
    -- here we forget about tyvars and covars, but mostly this is good enough.
    sub = mkOpenSubst emptyInScopeSet [ (v1, if isTyVar v2 then Type (mkTyVarTy v2) else if isCoVar v2 then Coercion (mkCoVarCo v2) else Var v2 ) | (v1, v2) <- rn]

#if MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
    substExpr' = substExpr
#else
    substExpr' = substExpr empty
#endif
