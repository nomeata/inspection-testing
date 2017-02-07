{-# LANGUAGE CPP #-}
module GHC.Proof.Plugin where


import Data.Maybe
import Control.Monad
import System.Exit

import GhcPlugins
import Simplify
import CoreStats
import SimplMonad
import OccurAnal
import FamInstEnv
import SimplEnv
import CSE

-- import GHC.Proof

plugin :: Plugin
plugin = defaultPlugin { installCoreToDos = install }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ (simpl:xs) = return $ simpl: pass : xs
  where pass = CoreDoPluginPass "GHC.Proof" proofPass


type Task = (SDoc, Type, CoreExpr, CoreExpr)

findProofTasks :: ModGuts -> CoreM [Task]
findProofTasks guts = return $ mapMaybe findProofTask (mg_binds guts)


findProofTask :: CoreBind -> Maybe Task
findProofTask (NonRec name (Var v `App` Type ty `App` e1 `App` e2))
    | isProof (idName v) = Just (ppr name, ty, e1,e2)
findProofTask _ = Nothing


isProof :: Name -> Bool
isProof n =
    occNameString oN == "proof" &&
    moduleNameString (moduleName (nameModule n)) == "GHC.Proof"
 || occNameString oN == "===" &&
    moduleNameString (moduleName (nameModule n)) == "GHC.Proof"
  where oN = occName n


proofTask :: ModGuts -> Task -> CoreM Bool
proofTask guts (name, _ty, e1, e2) = do
    putMsg (text "GHC.Proof: Proving" <+> name <+> text "…")
    se1 <- simplify guts e1
    se2 <- simplify guts e2
    let differences = diffExpr False (mkRnEnv2 emptyInScopeSet) se1 se2

    if null differences
      then return True
      else do
        putMsg $
            text "Proof failed" $$
            nest 4 (hang (text "Simplified LHS" <> colon) 4 (ppr se1)) $$
            nest 4 (hang (text "Simplified RHS" <> colon) 4 (ppr se2)) $$
            nest 4 (text "Differences:") $$
            nest 4 (itemize differences)
        return False

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

    rule_base <- getRuleBase
    vis_orphs <- getVisibleOrphanMods
    let rule_base2 = extendRuleBaseList rule_base (mg_rules guts)
    let rule_env = RuleEnv rule_base2 vis_orphs
    -- pprTrace "rb" (ppr rule_base2) (return ())

    (expr', _) <- liftIO $ initSmpl dflags' rule_env emptyFamInstEnvs us sz $ do
            return expr >>= simplExpr (simplEnv 4) . occurAnalyseExpr
                        >>= simplExpr (simplEnv 4) . occurAnalyseExpr
                        >>= simplExpr (simplEnv 3) . occurAnalyseExpr
                        >>= simplExpr (simplEnv 3) . occurAnalyseExpr
                        >>= simplExpr (simplEnv 2) . occurAnalyseExpr
                        >>= simplExpr (simplEnv 2) . occurAnalyseExpr
                        >>= simplExpr (simplEnv 1) . occurAnalyseExpr
#if  __GLASGOW_HASKELL__ >= 801
                                                                      . cseOneExpr
#endif
                        >>= simplExpr (simplEnv 1) . occurAnalyseExpr
#if  __GLASGOW_HASKELL__ >= 801
                                                                      . cseOneExpr
#endif
    return expr'

simplEnv :: Int -> SimplEnv
simplEnv p = mkSimplEnv $ SimplMode { sm_names = ["GHC.Proof"]
                                    , sm_phase = Phase p
                                    , sm_rules = True
                                    , sm_inline = True
                                    , sm_eta_expand = True
                                    , sm_case_case = True }

proofPass :: ModGuts -> CoreM ModGuts
proofPass guts = do

    dflags <- getDynFlags
    when (optLevel dflags < 1) $
        warnMsg $ fsep $ map text $ words "GHC.Proof: Compilation without -O detected. Expect proofs to fail."


    tasks <- findProofTasks guts
    ok <- and <$> mapM (proofTask guts) tasks
    if ok
      then do
        putMsg $ text "GHC.Proof proved" <+> ppr (length tasks) <+> text "equalities"
        return guts
      else do
        errorMsg $ text "GHC.Proof could not prove all equalities"
        liftIO $ exitFailure -- kill the compiler. Is there a nicer way?
