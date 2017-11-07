-- | See "Test.Inspection".
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
module Test.Inspection.Plugin (plugin) where

import Data.Maybe
import Control.Monad
import System.Exit
import Data.Either
import Data.List
import qualified Data.Map as M
import Data.Bifunctor
import qualified Language.Haskell.TH.Syntax as TH

import GhcPlugins hiding (SrcLoc)
import Simplify
import CoreStats
import CoreMonad


import Test.Inspection.Internal (KeepAlive(..))
import Test.Inspection (Obligation(..), Property(..))

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
fepAliveAnnindNamingAnn _
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
prettyProperty mod target (EqualTo n2) = showTHName mod target ++ " === " ++ showTHName mod n2
prettyProperty mod target (NoType t)   = showTHName mod target ++ " `hasNoType` " ++ showTHName mod t

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
lookupNameInGuts guts n = find ((== n) . getName . fst) (flattenBinds (mg_binds guts))

checkProperty :: ModGuts -> TH.Name -> Property -> CoreM Result
checkProperty guts thn1 (EqualTo thn2) = do
    Just n1 <- thNameToGhcName thn1
    Just n2 <- thNameToGhcName thn2

    let p1 = lookupNameInGuts guts n1
    let p2 = lookupNameInGuts guts n2

    if | n1 == n2
       -> return Nothing
       -- Ok if one points to another
       | Just (v1, Var v2) <- p1, getName v2 == n2
       -> return Nothing
       | Just (v2, Var v1) <- p2, getName v1 == n1
       -> return Nothing
       -- OK if they have the same expression
       | Just (v1, e1) <- p1
       , Just (v2, e2) <- p2
       , e1 `eq` e2
       -> return Nothing
       -- Not ok if the expression differ
       | Just (v1, e1) <- p1
       , Just (v2, e2) <- p2
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
        Just (_ ,e) -> pure . Just $ do
            putMsgS $ "NoType is not implemented yet"


itemize :: [SDoc] -> SDoc
itemize = vcat . map (char '•' <+>)

proofPass :: ModGuts -> CoreM ModGuts
proofPass guts = do
    dflags <- getDynFlags
    when (optLevel dflags < 1) $
        warnMsg $ fsep $ map text $ words "Test.Inspection: Compilation without -O detected. Expect optimizations to fail."

    let (guts', obligations) = extractObligations guts
    ok <- and <$> mapM (checkObligation guts') obligations
    if ok
      then do
        let (m,n) = bimap length length $ partition expectFail obligations
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
