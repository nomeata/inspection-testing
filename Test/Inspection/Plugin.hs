-- | See "Test.Inspection".
--
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Inspection.Plugin (plugin) where

import Control.Monad
import System.Exit
import Data.Either
import Data.Maybe
import Data.Bifunctor
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH.Syntax as TH

import GhcPlugins hiding (SrcLoc)
import Outputable

import Test.Inspection.Internal (KeepAlive(..))
import Test.Inspection (Obligation(..), Property(..), Result(..))
import Test.Inspection.Core

-- | The plugin. It supports the option @-fplugin-opt=Test.Inspection.Plugin:keep-going@ to
-- ignore a failing build.
plugin :: Plugin
plugin = defaultPlugin { installCoreToDos = install }

data UponFailure = AbortCompilation | KeepGoing deriving Eq

data ResultTarget = PrintAndAbort | StoreAt Name

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install args passes = return $ passes ++ [pass]
  where
    pass = CoreDoPluginPass "Test.Inspection.Plugin" (proofPass upon_failure)
    upon_failure | "keep-going" `elem` args = KeepGoing
                 | otherwise                = AbortCompilation


extractObligations :: ModGuts -> (ModGuts, [(ResultTarget, Obligation)])
extractObligations guts = (guts', obligations)
  where
    (anns', obligations) = partitionMaybe findObligationAnn (mg_anns guts)
    anns_clean = filter (not . isKeepAliveAnn) anns'
    guts' = guts { mg_anns = anns_clean }

isKeepAliveAnn :: Annotation -> Bool
isKeepAliveAnn (Annotation (NamedTarget _) payload)
    | Just KeepAlive <- fromSerialized deserializeWithData payload
    = True
isKeepAliveAnn _
    = False

findObligationAnn :: Annotation -> Maybe (ResultTarget, Obligation)
findObligationAnn (Annotation (ModuleTarget _) payload)
    | Just obl <- fromSerialized deserializeWithData payload
    = Just (PrintAndAbort, obl)
findObligationAnn (Annotation (NamedTarget n) payload)
    | Just obl <- fromSerialized deserializeWithData payload
    = Just (StoreAt n, obl)
findObligationAnn _
    = Nothing

prettyObligation :: Module -> Obligation -> String -> String
prettyObligation mod (Obligation {..}) result =
    maybe "" myPrettySrcLoc srcLoc ++ ": " ++
    prettyProperty mod target property ++
    " " ++ result

prettyProperty :: Module -> TH.Name -> Property -> String
prettyProperty mod target (EqualTo n2 False)  = showTHName mod target ++ " === " ++ showTHName mod n2
prettyProperty mod target (EqualTo n2 True)   = showTHName mod target ++ " ==- " ++ showTHName mod n2
prettyProperty mod target (NoType t)          = showTHName mod target ++ " `hasNoType` " ++ showTHName mod t
prettyProperty mod target NoAllocation        = showTHName mod target ++ " does not allocate"

-- | Like show, but omit the module name if it is he current module
showTHName :: Module -> TH.Name -> String
showTHName mod (TH.Name occ (TH.NameQ m))
    | moduleNameString (moduleName mod) == TH.modString m = TH.occString occ
showTHName mod (TH.Name occ (TH.NameG _ _ m))
    | moduleNameString (moduleName mod) == TH.modString m = TH.occString occ
showTHName _ n = show n

data Stat = ExpSuccess | ExpFailure | UnexpSuccess | UnexpFailure | StoredResult
    deriving (Enum, Eq, Ord, Bounded)
type Stats = M.Map Stat Int

type Updates = [(Name, Result)]

tick :: Stat -> Stats
tick s = M.singleton s 1

checkObligation :: ModGuts -> (ResultTarget, Obligation) -> CoreM (Updates, Stats)
checkObligation guts (reportTarget, obl) = do

    res <- checkProperty guts (target obl) (property obl)
    case reportTarget of
        PrintAndAbort -> do
            category <- case (res, expectFail obl) of
                -- Property holds
                (Nothing, False) -> do
                    putMsgS $ prettyObligation (mg_module guts) obl expSuccess
                    return ExpSuccess
                (Nothing, True) -> do
                    putMsgS $ prettyObligation (mg_module guts) obl unexpSuccess
                    return UnexpSuccess
                -- Property does not hold
                (Just reportDoc, False) -> do
                    putMsgS $ prettyObligation (mg_module guts) obl unexpFailure
                    putMsg $ reportDoc
                    return UnexpFailure
                (Just _, True) -> do
                    putMsgS $ prettyObligation (mg_module guts) obl expFailure
                    return ExpFailure
            return ([], tick category)
        StoreAt name -> do
            dflags <- getDynFlags
            let result = case res of
                    Nothing -> Success $ showSDoc dflags $
                        text (prettyObligation (mg_module guts) obl expSuccess)
                    Just reportMsg -> Failure $ showSDoc dflags $
                        text (prettyObligation (mg_module guts) obl unexpFailure) $$
                        reportMsg
            pure ([(name, result)], tick StoredResult)

  where
    expSuccess   = "passed."
    unexpSuccess = "passed unexpectedly!"
    unexpFailure = "failed:"
    expFailure   = "failed expectedly."


type CheckResult = Maybe SDoc

lookupNameInGuts :: ModGuts -> Name -> Maybe (Var, CoreExpr)
lookupNameInGuts guts n = listToMaybe
    [ (v,e)
    | (v,e) <- flattenBinds (mg_binds guts)
    , getName v == n
    ]

updateNameInGuts :: Name -> CoreExpr -> ModGuts -> ModGuts
updateNameInGuts n expr guts =
    guts {mg_binds = map (updateNameInGut n expr) (mg_binds guts) }

updateNameInGut :: Name -> CoreExpr -> CoreBind -> CoreBind
updateNameInGut n e (NonRec v _) | getName v == n = NonRec v e
updateNameInGut _ _ bind                          = bind

checkProperty :: ModGuts -> TH.Name -> Property -> CoreM CheckResult
checkProperty guts thn1 (EqualTo thn2 ignore_types) = do
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
       | Just (v1, _) <- p1
       , Just (v2, _) <- p2
       , let slice1 = slice binds v1
       , let slice2 = slice binds v2
       -> if eqSlice ignore_types slice1 slice2
          -- OK if they have the same expression
          then return Nothing
          -- Not ok if the expression differ
          else pure . Just $ pprSliceDifference slice1 slice2
       -- Not ok if both names are bound externally
       | Nothing <- p1
       , Nothing <- p2
       -> pure . Just $ ppr n1 <+> text " and " <+> ppr n2 <+>
                text "are different external names"
  where
    binds = flattenBinds (mg_binds guts)

checkProperty guts thn (NoType tht) = do
    Just n <- thNameToGhcName thn
    Just t <- thNameToGhcName tht
    case lookupNameInGuts guts n of
        Nothing -> pure . Just $ ppr n <+> text "is not a local name"
        Just (v, _) -> case freeOfType (slice binds v) t of
            Just _ -> pure . Just $ pprSlice (slice binds v)
            Nothing -> pure Nothing
  where binds = flattenBinds (mg_binds guts)

checkProperty guts thn NoAllocation = do
    Just n <- thNameToGhcName thn
    case lookupNameInGuts guts n of
        Nothing -> pure . Just $ ppr n <+> text "is not a local name"
        Just (v, _) -> case doesNotAllocate (slice binds v) of
            Just (v',e') -> pure . Just $ nest 4 (ppr v' <+> text "=" <+> ppr e')
            Nothing -> pure Nothing
  where binds = flattenBinds (mg_binds guts)

storeResults :: Updates -> ModGuts -> CoreM ModGuts
storeResults = flip (foldM (flip (uncurry go)))
  where
    go :: Name -> Result -> ModGuts -> CoreM ModGuts
    go name res guts = do
        e <- resultToExpr res
        pure $ updateNameInGuts name e guts

dcExpr :: TH.Name -> CoreM CoreExpr
dcExpr thn = do
    Just name <- thNameToGhcName thn
    dc <- lookupDataCon name
    pure $ Var (dataConWrapId dc)

resultToExpr :: Result -> CoreM CoreExpr
resultToExpr (Success s) = App <$> dcExpr 'Success <*> mkStringExpr s
resultToExpr (Failure s) = App <$> dcExpr 'Failure <*> mkStringExpr s

proofPass :: UponFailure -> ModGuts -> CoreM ModGuts
proofPass upon_failure guts = do
    dflags <- getDynFlags
    when (optLevel dflags < 1) $
        warnMsg $ fsep $ map text $ words "Test.Inspection: Compilation without -O detected. Expect optimizations to fail."

    let (guts', obligations) = extractObligations guts
    (toStore, stats) <- (concat `bimap` M.unionsWith (+)) . unzip <$>
        mapM (checkObligation guts') obligations
    let n = sum stats :: Int

    guts'' <- storeResults toStore  guts'

    let q :: Stat -> Int
        q s = fromMaybe 0 $ M.lookup s stats

    let summary_message = nest 2 $
            vcat [ nest 2 (desc s) Outputable.<> colon <+> ppr (q s)
                 | s <- [minBound..maxBound], q s > 0 ]

    -- Only print a message if there are some compile-time results to report
    unless (q StoredResult == n) $ do
        if q ExpSuccess + q ExpFailure + q StoredResult == n
        then putMsg $ text "inspection testing successful" $$ summary_message
        else case upon_failure of
            AbortCompilation -> do
                errorMsg $ text "inspection testing unsuccessful" $$ summary_message
                liftIO $ exitFailure -- kill the compiler. Is there a nicer way?
            KeepGoing -> do
                warnMsg $ text "inspection testing unsuccessful" $$ summary_message

    return guts''


desc :: Stat -> SDoc
desc ExpSuccess   = text "  expected successes"
desc UnexpSuccess = text "unexpected successes"
desc ExpFailure   = text "   expected failures"
desc UnexpFailure = text " unexpected failures"
desc StoredResult = text "      results stored"

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
