-- | See "Test.Inspection".
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
module Test.Inspection.Plugin (plugin) where

import Data.Maybe
import Control.Monad
import System.Exit

import GhcPlugins
import Simplify
import CoreStats
import CoreMonad
import SimplMonad
import OccurAnal
import FamInstEnv
import SimplEnv
import CSE

-- import Test.Inspection

plugin :: Plugin
plugin = defaultPlugin { installCoreToDos = install }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ (simpl:xs) = return $ simpl: myOccurPass : pass : xs
  where pass = CoreDoPluginPass "Test.Inspection" proofPass
        myOccurPass = CoreDoPluginPass "Test.Inspection Occur" occurPass


type Task = (SDoc, Bool, CoreExpr, CoreExpr)

findProofTasks :: ModGuts -> CoreM [Task]
findProofTasks guts =
    return $ mapMaybe findProofTaskRule (mg_rules guts)

findProofTaskRule :: CoreRule -> Maybe Task
findProofTaskRule (Rule{..})
    | (Var v `App` Type _ `App` e1 `App` e2) <- ru_rhs
    , isProof (idName v)
    = Just (ppr ru_name, True, e1,e2)
findProofTaskRule (Rule{..})
    | (Var v `App` Type _ `App` e1 `App` e2) <- ru_rhs
    , isNonProof (idName v)
    = Just (ppr ru_name, False, e1,e2)
findProofTaskRule _ = Nothing

isProof :: Name -> Bool
isProof n =
    occNameString oN == "proof" &&
    moduleNameString (moduleName (nameModule n)) == "Test.Inspection"
 || occNameString oN == "===" &&
    moduleNameString (moduleName (nameModule n)) == "Test.Inspection"
  where oN = occName n

isNonProof :: Name -> Bool
isNonProof n =
    occNameString oN == "non_proof" &&
    moduleNameString (moduleName (nameModule n)) == "Test.Inspection"
 || occNameString oN == "=/=" &&
    moduleNameString (moduleName (nameModule n)) == "Test.Inspection"
  where oN = occName n


proveTask :: ModGuts -> Task -> CoreM Bool
proveTask guts (name, really, e1, e2) = do
    if really
      then putMsg (text "Test.Inspection: Proving" <+> name <+> text "…")
      else putMsg (text "Test.Inspection: Not proving" <+> name <+> text "…")

    se1 <- simplify guts e1
    se2 <- simplify guts e2
    let differences = diffExpr False (mkRnEnv2 emptyInScopeSet) se1 se2

    if really
      then
        if null differences
          then return True
          else do
            putMsg $
                text "Proof failed" $$
                nest 4 (hang (text "Simplified LHS" <> colon) 4 (ppr se1)) $$
                nest 4 (hang (text "Simplified RHS" <> colon) 4 (ppr se2))
                -- nest 4 (text "Differences:") $$
                -- nest 4 (itemize differences)
            return False
      else
        if null differences
          then do
            putMsg $ text "Proof succeeded unexpectedly"
            return False
          else do
            return True

itemize :: [SDoc] -> SDoc
itemize = vcat . map (char '•' <+>)

simplify :: ModGuts -> CoreExpr -> CoreM CoreExpr
simplify guts expr = do
    dflags <- getDynFlags

#if  __GLASGOW_HASKELL__ >= 801
    let dflags' = dflags { ufUseThreshold = 1000, ufVeryAggressive = True } --yeeha!
#else
    let dflags' = dflags { ufUseThreshold = 1000 }
#endif
    us <- liftIO $ mkSplitUniqSupply 's'
    let sz = exprSize expr

    hpt_rule_base <- getRuleBase
    hsc_env <- getHscEnv
    eps <- liftIO $ hscEPS hsc_env
    let rule_base1 = unionRuleBase hpt_rule_base (eps_rule_base eps)
        rule_base2 = extendRuleBaseList rule_base1 (mg_rules guts)
    vis_orphs <- getVisibleOrphanMods
    let rule_env = RuleEnv rule_base2 vis_orphs
    let in_scope = bindersOfBinds (mg_binds guts)

    (expr', _) <- liftIO $ initSmpl dflags' rule_env emptyFamInstEnvs us sz $ do
            return expr >>= simplExpr (simplEnv in_scope 4) . occurAnalyseExpr
                        >>= simplExpr (simplEnv in_scope 4) . occurAnalyseExpr
                        >>= simplExpr (simplEnv in_scope 3) . occurAnalyseExpr
                        >>= simplExpr (simplEnv in_scope 3) . occurAnalyseExpr
                        >>= simplExpr (simplEnv in_scope 2) . occurAnalyseExpr
                        >>= simplExpr (simplEnv in_scope 2) . occurAnalyseExpr
                        >>= simplExpr (simplEnv in_scope 2) . occurAnalyseExpr
                        >>= simplExpr (simplEnv in_scope 1) . occurAnalyseExpr . cseOneExpr'
                        >>= simplExpr (simplEnv in_scope 1) . occurAnalyseExpr . cseOneExpr'
                        >>= simplExpr (simplEnv in_scope 0) . occurAnalyseExpr . cseOneExpr'
                        >>= simplExpr (simplEnv in_scope 0) . occurAnalyseExpr . cseOneExpr'
    return expr'

#if  __GLASGOW_HASKELL__ >= 801
cseOneExpr' = cseOneExpr
#else
cseOneExpr' = id
#endif

simplEnv :: [Var] -> Int -> SimplEnv
simplEnv vars p = env1
  where
    env1 = addNewInScopeIds env0 vars
    env0 =  mkSimplEnv $ SimplMode { sm_names = ["Test.Inspection"]
                                   , sm_phase = Phase p
                                   , sm_rules = True
                                   , sm_inline = True
                                   , sm_eta_expand = True
                                   , sm_case_case = True }

proofPass :: ModGuts -> CoreM ModGuts
proofPass guts = do

    dflags <- getDynFlags
    when (optLevel dflags < 1) $
        warnMsg $ fsep $ map text $ words "Test.Inspection: Compilation without -O detected. Expect proofs to fail."


    tasks <- findProofTasks guts
    ok <- and <$> mapM (proveTask guts) tasks
    if ok
      then do
        let n = length [ () | (_, True, _, _) <- tasks ]
        let m = length [ () | (_, False, _, _) <- tasks ]
        putMsg $ text "Test.Inspection proved" <+> ppr n <+> text "equalities"
        return guts
      else do
        errorMsg $ text "Test.Inspection could not prove all equalities"
        liftIO $ exitFailure -- kill the compiler. Is there a nicer way?

occurPass :: PluginPass
occurPass mg@(ModGuts { mg_module = this_mod
                            , mg_rdr_env = rdr_env
                            , mg_deps = deps
                            , mg_binds = binds, mg_rules = rules
                            , mg_fam_inst_env = fam_inst_env })
 = do let binds' = occurAnalysePgm this_mod (const True) rules [] emptyVarSet  binds
      return mg

