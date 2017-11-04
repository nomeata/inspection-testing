-- | See "Test.Inspection".
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
module Test.Inspection.Plugin (plugin) where

import Data.Maybe
import Control.Monad
import System.Exit
import Data.Either
import Data.List
import qualified Data.Map as M
import Data.Bifunctor
import qualified Language.Haskell.TH.Syntax as TH

import GhcPlugins
import Simplify
import CoreStats
import CoreMonad


import Test.Inspection.Internal (Obligation(..), THName(..))

plugin :: Plugin
plugin = defaultPlugin { installCoreToDos = install }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ passes = return $ passes ++ [pass]
  where pass = CoreDoPluginPass "Test.Inspection" proofPass


type Task = (SDoc, Bool, Name, Name)

extractTasks :: ModGuts -> (ModGuts, [Task])
extractTasks guts = (guts { mg_rules = rules', mg_anns = anns' }, tasks)
  where
    rules' = mg_rules guts
    -- (rules', tasks) = partitionMaybe findTaskRule (mg_rules guts)
    ((anns', tasks), table) = bimap (partitionMaybe (findTaskAnn table)) mconcat $
                            partitionMaybe findNamingAnn (mg_anns guts)

type THNameEnv = M.Map TH.Name Name

findNamingAnn :: Annotation -> Maybe THNameEnv
findNamingAnn (Annotation (NamedTarget coreName) payload)
    | Just (THName thName) <- fromSerialized deserializeWithData payload
    = Just $ M.singleton thName coreName
findNamingAnn _
    = Nothing

findTaskAnn :: THNameEnv -> Annotation -> Maybe Task
findTaskAnn env (Annotation (ModuleTarget _) payload)
    | Just (Equal really thn1 thn2) <- fromSerialized deserializeWithData payload
    , ~(Just n1) <- M.lookup thn1 env
    , ~(Just n2) <- M.lookup thn2 env
    = Just (text "foo", really, n1, n2)
findTaskAnn _ _
    = Nothing

{- Alternative transport mechanism in RULES
findTaskRule :: CoreRule -> Maybe Task
findTaskRule (Rule{..})
    | (Var v `App` Type _ `App` e1 `App` e2) <- ru_rhs
    , (Var v1, args1) <- collectArgs e1
    , all isTypeArg args1
    , (Var v2, args2) <- collectArgs e2
    , all isTypeArg args2
    , Just really <- isProof (idName v)
    = Just (ppr ru_name, really, getName v1, getName v2)
findTaskRule _ = Nothing

isProof :: Name -> Maybe Bool
isProof n
    | occNameString oN == "proof"
    , moduleNameString (moduleName (nameModule n)) == "Test.Inspection"
    = Just True
    | occNameString oN == "==="
    , moduleNameString (moduleName (nameModule n)) == "Test.Inspection"
    = Just True
    | occNameString oN == "non_proof"
    , moduleNameString (moduleName (nameModule n)) == "Test.Inspection"
    = Just False
    | occNameString oN == "=/="
    , moduleNameString (moduleName (nameModule n)) == "Test.Inspection"
    = Just False
    | otherwise
    = Nothing
  where oN = occName n
-}

checkTask :: ModGuts -> Task -> CoreM Bool
checkTask guts (name, really, n1, n2) = do
    let task_descr = ppr n1 <+> text "===" <+> ppr n2

    if really
      then putMsg $ text "Test.Inspection:     Checking" <+> task_descr
      else putMsg $ text "Test.Inspection: Not checking" <+> task_descr

    case ( find ((== n1) . getName . fst) (flattenBinds (mg_binds guts))
         , find ((== n2) . getName . fst) (flattenBinds (mg_binds guts))) of
        (Nothing, _) -> do
            putMsg $ text "Cannot find" <+> ppr n1
            return False
        (_, Nothing) -> do
            putMsg $ text "Cannot find" <+> ppr n2
            return False
        (Just (v1,e1), Just (v2,e2)) -> do

            let ok = or [ e1 `eq` e2
                        , e1 `eq` Var v2
                        , Var v1 `eq` e2
                        ]

            result e1 e2 really ok
  where
    result _ _ True True = do
        return True
    result _ _ False True = do
        putMsg $ text "Obligation passes unexpectedly"
        return False
    result e1 e2 True False = do
        putMsg $
            text "Obligation failes" $$
            nest 4 (hang (text "LHS" <> colon) 4 (ppr e1)) $$
            nest 4 (hang (text "RHS" <> colon) 4 (ppr e2))
        return False
    result _ _ False False  = do
        return True

    eq = eqExpr emptyInScopeSet

itemize :: [SDoc] -> SDoc
itemize = vcat . map (char '•' <+>)

proofPass :: ModGuts -> CoreM ModGuts
proofPass guts = do
    dflags <- getDynFlags
    when (optLevel dflags < 1) $
        warnMsg $ fsep $ map text $ words "Test.Inspection: Compilation without -O detected. Expect proofs to fail."

    let (guts', tasks) = extractTasks guts
    ok <- and <$> mapM (checkTask guts') tasks
    if ok
      then do
        let n = length [ () | (_, True, _, _) <- tasks ]
        let m = length [ () | (_, False, _, _) <- tasks ]
        putMsg $ text "Test.Inspection proved" <+> ppr n <+> text "equalities"
        return guts'
      else do
        errorMsg $ text "Test.Inspection could not prove all equalities"
        liftIO $ exitFailure -- kill the compiler. Is there a nicer way?


partitionMaybe :: (a -> Maybe b) -> [a] -> ([a], [b])
partitionMaybe f = partitionEithers . map (\x -> maybe (Left x) Right (f x))
