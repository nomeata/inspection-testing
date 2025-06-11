-- | See "Test.Inspection".
--
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module Test.Inspection.Plugin
  ( plugin
  , checkProperty
  , CheckResult(..)
  , prettyProperty
  ) where

import Control.Monad
import System.Exit
import Data.Either
import Data.Maybe
import Data.Bifunctor
import Data.List
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH.Syntax as TH

#if MIN_VERSION_ghc(9,4,0)
import GHC.Types.Error
import GHC.Driver.Session
#endif

#if MIN_VERSION_ghc(9,0,0)
import GHC.Plugins hiding (SrcLoc)
import GHC.Utils.Outputable as Outputable
#else
import GhcPlugins hiding (SrcLoc)
import Outputable
#endif

#if MIN_VERSION_ghc(9,2,0)
import GHC.Types.TyThing
#endif

import Test.Inspection (Obligation(..), Equivalence (..), Property(..), Result(..))
import Test.Inspection.Core

-- | The plugin. It supports some options:
--
-- * @-fplugin-opt=Test.Inspection.Plugin:keep-going@ to keep building despite failing obligations
-- * @-fplugin-opt=Test.Inspection.Plugin:keep-going-O0@ to keep building despite failing obligations, when optimisations are off
-- * @-fplugin-opt=Test.Inspection.Plugin:skip-O0@ to skip performing inspections when optimisations are off
-- * @-fplugin-opt=Test.Inspection.Plugin:quiet@ to be silent if all obligations are fulfilled
--
-- It makes sense to enable only one of @keep-going@, @keep-going-O0@ and
-- @skip-O0@ at a time. @skip-O0@ is useful when working with GHCi, to suppress
-- inspection failure messages and eliminate the overhead of inspection when
-- loading.


plugin :: Plugin
plugin = defaultPlugin
    { installCoreToDos = install
#if __GLASGOW_HASKELL__ >= 806
    , pluginRecompile = \_args -> pure NoForceRecompile
#endif
    }

data UponFailure = AbortCompilation | KeepGoingO0 | SkipO0 | KeepGoing deriving Eq

data ReportingMode = Verbose | Quiet deriving Eq

data ResultTarget = PrintAndAbort | StoreAt Name

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install args passes = return $ passes ++ [pass]
  where
    pass = CoreDoPluginPass "Test.Inspection.Plugin" (proofPass upon_failure report)
    upon_failure | "keep-going" `elem` args    = KeepGoing
                 | "keep-going-O0" `elem` args = KeepGoingO0
                 | "skip-O0" `elem` args       = SkipO0
                 | otherwise                   = AbortCompilation
    report | "quiet" `elem` args = Quiet
           | otherwise           = Verbose

extractObligations :: ModGuts -> (ModGuts, [(ResultTarget, Obligation)])
extractObligations guts = (guts', obligations)
  where
    (anns_clean, obligations) = partitionMaybe findObligationAnn (mg_anns guts)
    guts' = guts { mg_anns = anns_clean }

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
    maybe "" myPrettySrcLoc srcLoc ++ ": " ++ name ++ " " ++ result
  where
    name = case testName of
        Just n -> n
        Nothing -> prettyProperty (showTHName mod) target property

prettyProperty :: (TH.Name -> String) -> TH.Name -> Property -> String
prettyProperty showName target = \case
  EqualTo n2 eqv   -> showName target ++ " " ++ showEquiv eqv ++ " " ++ showName n2
  NoTypes [t]      -> showName target ++ " `hasNoType` " ++ showName t
  NoTypes ts       -> showName target ++ " mentions none of " ++ intercalate ", " (map showName ts)
  NoAllocation     -> showName target ++ " does not allocate"
  NoTypeClasses [] -> showName target ++ " does not contain dictionary values"
  NoTypeClasses ts -> showName target ++ " does not contain dictionary values except of " ++ intercalate ", " (map showName ts)
  NoUseOf ns       -> showName target ++ " uses none of " ++ intercalate ", " (map showName ns)
  CoreOf           -> showName target ++ " core dump" -- :)
  where
    showEquiv StrictEquiv              = "==="
    showEquiv IgnoreTypesAndTicksEquiv = "==-"
    showEquiv UnorderedLetsEquiv       = "==~"

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

checkObligation :: ReportingMode -> ModGuts -> (ResultTarget, Obligation) -> CoreM (Updates, Stats)
checkObligation report guts (reportTarget, obl) = do

    res <- checkProperty guts (target obl) (property obl)
    case reportTarget of
        PrintAndAbort -> do
            category <- case (res, expectFail obl) of
                -- Property holds
                (ResSuccess, False) -> do
                    unless (report == Quiet) $
                        putMsgS $ prettyObligation (mg_module guts) obl expSuccess
                    return ExpSuccess
                (ResSuccess, True) -> do
                    putMsgS $ prettyObligation (mg_module guts) obl unexpSuccess
                    return UnexpSuccess
                -- Property holds, with extra message
                (ResSuccessWithMessage reportDoc, False) -> do
                    unless (report == Quiet) $ do
                        putMsgS $ prettyObligation (mg_module guts) obl expSuccess
                        putMsg reportDoc
                    return ExpSuccess
                (ResSuccessWithMessage reportDoc, True) -> do
                    putMsgS $ prettyObligation (mg_module guts) obl unexpSuccess
                    putMsg reportDoc
                    return UnexpSuccess
                -- Property does not hold
                (ResFailure reportDoc, False) -> do
                    putMsgS $ prettyObligation (mg_module guts) obl unexpFailure
                    putMsg $ reportDoc
                    return UnexpFailure
                (ResFailure _, True) -> do
                    unless (report == Quiet) $
                        putMsgS $ prettyObligation (mg_module guts) obl expFailure
                    return ExpFailure
            return ([], tick category)
        StoreAt name -> do
            dflags <- getDynFlags
            let result = case res of
                    ResSuccess -> Success $ showSDoc dflags $
                        text (prettyObligation (mg_module guts) obl expSuccess)
                    ResSuccessWithMessage msg -> Success $ showSDoc dflags $
                        text (prettyObligation (mg_module guts) obl expSuccess) $$
                        msg
                    ResFailure reportMsg -> Failure $ showSDoc dflags $
                        text (prettyObligation (mg_module guts) obl unexpFailure) $$
                        reportMsg
            pure ([(name, result)], tick StoredResult)

  where
    expSuccess   = "passed."
    unexpSuccess = "passed unexpectedly!"
    unexpFailure = "failed:"
    expFailure   = "failed expectedly."


data CheckResult
    = ResSuccess
    | ResSuccessWithMessage SDoc
    | ResFailure SDoc

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
    n1 <- fromTHName thn1
    n2 <- fromTHName thn2

    let p1 = lookupNameInGuts guts n1
    let p2 = lookupNameInGuts guts n2

    if | n1 == n2
       -> pure ResSuccess
       -- Ok if one points to another
       | Just (_, Var other) <- p1, getName other == n2
       -> pure ResSuccess
       | Just (_, Var other) <- p2, getName other == n1
       -> pure ResSuccess
       | Just (v1, _) <- p1
       , Just (v2, _) <- p2
       , let slice1 = slice binds v1
       , let slice2 = slice binds v2
       -> case eqSlice' ignore_types slice1 slice2 of
            -- OK if they have the same expression
            Right _ -> pure ResSuccess
          -- Not ok if the expression differ
            Left err -> pure . ResFailure $ err $$ pprSliceDifference slice1 slice2
       -- Not ok if both names are bound externally
       | Nothing <- p1
       , Nothing <- p2
       -> pure . ResFailure $ ppr n1 <+> text " and " <+> ppr n2 <+>
                text "are different external names"
       | Nothing <- p1
       -> pure . ResFailure $ ppr n1 <+> text "is an external name"
       | Nothing <- p2
       -> pure . ResFailure $ ppr n2 <+> text "is an external name"
  where
    binds = flattenBinds (mg_binds guts)

checkProperty guts thn (NoUseOf thns) = do
    n <- fromTHName thn
    ns <- mapM fromTHName thns
    case lookupNameInGuts guts n of
        Nothing -> pure . ResFailure $ ppr n <+> text "is not a local name"
        Just (v, _) -> case freeOfTerm (slice binds v) ns of
            Just _ -> pure . ResFailure $ pprSlice (slice binds v)
            Nothing -> pure ResSuccess
  where binds = flattenBinds (mg_binds guts)

checkProperty guts thn (NoTypes thts) = do
    n <- fromTHName thn
    ts <- mapM fromTHName thts
    case lookupNameInGuts guts n of
        Nothing -> pure . ResFailure $ ppr n <+> text "is not a local name"
        Just (v, _) -> case freeOfType (slice binds v) ts of
            Just _ -> pure . ResFailure $ pprSlice (slice binds v)
            Nothing -> pure ResSuccess
  where binds = flattenBinds (mg_binds guts)

checkProperty guts thn NoAllocation = do
    n <- fromTHName thn
    case lookupNameInGuts guts n of
        Nothing -> pure . ResFailure $ ppr n <+> text "is not a local name"
        Just (v, _) -> case doesNotAllocate (slice binds v) of
            Just (v',e') -> pure . ResFailure $ nest 4 (ppr v' <+> text "=" <+> ppr e')
            Nothing -> pure ResSuccess
  where binds = flattenBinds (mg_binds guts)

checkProperty guts thn (NoTypeClasses thts) = do
    n <- fromTHName thn
    ts <- mapM fromTHName thts
    case lookupNameInGuts guts n of
        Nothing -> pure . ResFailure $ ppr n <+> text "is not a local name"
        Just (v, _) -> case doesNotContainTypeClasses (slice binds v) ts of
            Just (v',e',tc) -> pure . ResFailure
                $ nest 4 $ vcat
                    [ text "Found type classes: " <+> ppr tc
                    , ppr v' <+> text "=" <+> ppr e'
                    ]
            Nothing -> pure ResSuccess
  where binds = flattenBinds (mg_binds guts)

checkProperty guts thn CoreOf = do
    n <- fromTHName thn
    case lookupNameInGuts guts n of
        Nothing -> pure . ResFailure $ ppr n <+> text "is not a local name"
        Just (v, _) -> do
            let s = slice binds v
            pure $ ResSuccessWithMessage $ nest 4 $ pprSlice s
  where binds = flattenBinds (mg_binds guts)

fromTHName :: TH.Name -> CoreM Name
fromTHName thn = thNameToGhcName thn >>= \case
    Nothing -> do
        errorMsg $ text "Could not resolve TH name" <+> text (show thn)
        liftIO $ exitFailure -- kill the compiler. Is there a nicer way?
    Just n -> return n

storeResults :: Updates -> ModGuts -> CoreM ModGuts
storeResults = flip (foldM (flip (uncurry go)))
  where
    go :: Name -> Result -> ModGuts -> CoreM ModGuts
    go name res guts = do
        e <- resultToExpr res
        pure $ updateNameInGuts name e guts

dcExpr :: TH.Name -> CoreM CoreExpr
dcExpr thn = do
    name <- fromTHName thn
    dc <- lookupDataCon name
    pure $ Var (dataConWrapId dc)

resultToExpr :: Result -> CoreM CoreExpr
resultToExpr (Success s) = App <$> dcExpr 'Success <*> mkStringExpr s
resultToExpr (Failure s) = App <$> dcExpr 'Failure <*> mkStringExpr s

proofPass :: UponFailure -> ReportingMode -> ModGuts -> CoreM ModGuts
proofPass upon_failure report guts = do
    case upon_failure of
        SkipO0 -> pure guts
        _ -> do
            let (guts', obligations) = extractObligations guts
            (toStore, stats) <- (concat `bimap` M.unionsWith (+)) . unzip <$>
                mapM (checkObligation report guts') obligations
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
                then unless (report == Quiet) $
                        putMsg $ text "inspection testing successful" $$ summary_message
                else do
                    errorMsg $ text "inspection testing unsuccessful" $$ summary_message
                    case upon_failure of
                        KeepGoing           -> return ()
                        _                   -> liftIO $ exitFailure -- kill the compiler. Is there a nicer way?

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
