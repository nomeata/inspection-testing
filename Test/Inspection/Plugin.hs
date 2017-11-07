-- | See "Test.Inspection".
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
module Test.Inspection.Plugin (plugin) where

import Control.Monad
import System.Exit
import Data.Either
import Data.Maybe
import Data.List
import Data.Bifunctor
import qualified Language.Haskell.TH.Syntax as TH

import GhcPlugins hiding (SrcLoc)

import Test.Inspection.Internal (KeepAlive(..))
import Test.Inspection (Obligation(..), Property(..))
import Test.Inspection.Core

plugin :: Plugin
plugin = defaultPlugin { installCoreToDos = install }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ passes = return $ passes ++ [pass]
  where pass = CoreDoPluginPass "Test.Inspection" proofPass


extractObligations :: ModGuts -> (ModGuts, [Obligation])
extractObligations guts = (guts { mg_rules = rules', mg_anns = anns_clean }, obligations)
  where
    rules' = mg_rules guts
    (anns', obligations) = partitionMaybe findObligationAnn (mg_anns guts)
    anns_clean = filter (not . isKeepAliveAnn) anns'

isKeepAliveAnn :: Annotation -> Bool
isKeepAliveAnn (Annotation (NamedTarget _) payload)
    | Just KeepAlive <- fromSerialized deserializeWithData payload
    = True
isKeepAliveAnn _
    = False

findObligationAnn :: Annotation -> Maybe Obligation
findObligationAnn (Annotation (ModuleTarget _) payload)
    | Just obl <- fromSerialized deserializeWithData payload
    = Just obl
findObligationAnn _
    = Nothing

prettyObligation :: Module -> Obligation -> String
prettyObligation mod (Obligation {..}) =
    maybe "" myPrettySrcLoc srcLoc ++ ": " ++
    "inspecting " ++ prettyProperty mod target property ++
    (if expectFail then " (failure expected)" else "")

prettyProperty :: Module -> TH.Name -> Property -> String
prettyProperty mod target (EqualTo n2)        = showTHName mod target ++ " === " ++ showTHName mod n2
prettyProperty mod target (NoType t)          = showTHName mod target ++ " `hasNoType` " ++ showTHName mod t
prettyProperty mod target NoAllocation        = showTHName mod target ++ " does not allocate"
prettyProperty mod target NoAllocationInLoop  = showTHName mod target ++ " does not allocate in a loop"

-- | Like show, but omit the module name if it is he current module
showTHName :: Module -> TH.Name -> String
showTHName mod (TH.Name occ (TH.NameQ m))
    | moduleNameString (moduleName mod) == TH.modString m = TH.occString occ
showTHName mod (TH.Name occ (TH.NameG _ _ m))
    | moduleNameString (moduleName mod) == TH.modString m = TH.occString occ
showTHName _ n = show n

checkObligation :: ModGuts -> Obligation -> CoreM Bool
checkObligation guts obl = do
    putMsgS $ prettyObligation (mg_module guts) obl

    res <- checkProperty guts (target obl) (property obl)

    case (res, expectFail obl) of
        -- Property holds
        (Nothing, False) -> do
            return True
        (Nothing, True) -> do
            putMsgS "Obligation passes unexpectedly"
            return False
        -- Property does not hold
        (Just reportMsg, False) -> do
            putMsgS "Obligation fails"
            reportMsg
            return False
        (Just _, True) -> do
            return True

type Result =  Maybe (CoreM ())

lookupNameInGuts :: ModGuts -> Name -> Maybe (Var, CoreExpr)
lookupNameInGuts guts n = listToMaybe
    [ (v,e)
    | (v,e) <- flattenBinds (mg_binds guts)
    , getName v == n
    ]

checkProperty :: ModGuts -> TH.Name -> Property -> CoreM Result
checkProperty guts thn1 (EqualTo thn2) = do
    Just n1 <- thNameToGhcName thn1
    Just n2 <- thNameToGhcName thn2

    let p1 = lookupNameInGuts guts n1
    let p2 = lookupNameInGuts guts n2

    if | n1 == n2
       -> return Nothing
       -- Ok if one points to another
       | Just (_, Var other) <- p1, getName other == n2
       -> return Nothing
       | Just (_, Var other) <- p2, getName other == n1
       -> return Nothing
       -- OK if they have the same expression
       | Just (_, e1) <- p1
       , Just (_, e2) <- p2
       , e1 `eq` e2
       -> return Nothing
       -- Not ok if the expression differ
       | Just (_, e1) <- p1
       , Just (_, e2) <- p2
       -> pure . Just $ do
            putMsg $
                nest 4 (hang (text "LHS" <> colon) 4 (ppr e1)) $$
                nest 4 (hang (text "RHS" <> colon) 4 (ppr e2))
       -- Not ok if both names are bound externally
       | Nothing <- p1
       , Nothing <- p2
       -> pure . Just $ do
            putMsg $ ppr n1 <+> text " and " <+> ppr n2 <+>
                text "are different external names"
  where
    eq = eqExpr emptyInScopeSet

checkProperty guts thn (NoType tht) = do
    Just n <- thNameToGhcName thn
    Just t <- thNameToGhcName tht
    case lookupNameInGuts guts n of
        Nothing -> pure . Just $ do
            putMsg $ ppr n <+> text "is not a local name"
        Just (_ ,e) | freeOfType t e -> pure Nothing
                    | otherwise -> pure . Just $ putMsg $ nest 4 (ppr e)

checkProperty _guts _thn NoAllocation = do
        pure . Just $ do
            putMsgS "not implemented"
checkProperty _guts _thn NoAllocationInLoop = do
        pure . Just $ do
            putMsgS "not implemented"

proofPass :: ModGuts -> CoreM ModGuts
proofPass guts = do
    dflags <- getDynFlags
    when (optLevel dflags < 1) $
        warnMsg $ fsep $ map text $ words "Test.Inspection: Compilation without -O detected. Expect optimizations to fail."

    let (guts', obligations) = extractObligations guts
    ok <- and <$> mapM (checkObligation guts') obligations
    if ok
      then do
        let (_m,n) = bimap length length $ partition expectFail obligations
        putMsg $ text "Test.Inspection tested" <+> ppr n <+>
                 text "obligation" <> (if n == 1 then empty else text "s")
        return guts'
      else do
        errorMsg $ text "inspection testing unsuccessful"
        liftIO $ exitFailure -- kill the compiler. Is there a nicer way?


partitionMaybe :: (a -> Maybe b) -> [a] -> ([a], [b])
partitionMaybe f = partitionEithers . map (\x -> maybe (Left x) Right (f x))

-- | like prettySrcLoc, but omits the module name
myPrettySrcLoc :: TH.Loc -> String
myPrettySrcLoc TH.Loc {..}
  = foldr (++) ""
      [ loc_filename, ":"
      , show (fst loc_start), ":"
      , show (snd loc_start)
      ]
