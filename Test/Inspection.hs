-- |
-- Description : Inspection Testing for Haskell
-- Copyright   : (c) Joachim Breitner, 2017
-- License     : MIT
-- Maintainer  : mail@joachim-breitner.de
-- Portability : GHC specifc
--
-- This module supports the accompanying GHC plugin "Test.Inspection.Plugin" and adds
-- to GHC the ability to do inspection testing.

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
module Test.Inspection (
    -- * Synopsis
    -- $synposis

    -- * Registering obligations
    inspect,
    inspectTest,
    Result(..),
    -- * Defining obligations
    Obligation(..), mkObligation, Property(..),
    -- * Convenience functions
    -- $convenience
    (===), (==-), (=/=), hasNoType, hasNoGenerics,
    hasNoTypeClasses, hasNoTypeClassesExcept,
) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Quasi(qNewName), liftData, addTopDecls)
#if MIN_VERSION_GLASGOW_HASKELL(8,5,0,0)
import Language.Haskell.TH.Syntax (getQ, putQ) -- only for needsPluginQ
#endif
import Data.Data
import GHC.Exts (lazy)
import GHC.Generics (V1(), U1(), M1(), K1(), (:+:), (:*:), (:.:), Rec1, Par1)

{- $synposis

To use inspection testing, you need to

 1. enable the @TemplateHaskell@ language extension
 2. load the plugin using @-fplugin Test.Inspection.Plugin@
 3. declare your proof obligations using 'inspect' or 'inspectTest'

An example module is

@
{&#45;\# LANGUAGE TemplateHaskell \#&#45;}
{&#45;\# OPTIONS_GHC -O -fplugin Test.Inspection.Plugin \#&#45;}
module Simple where

import Test.Inspection
import Data.Maybe

lhs, rhs :: (a -> b) -> Maybe a -> Bool
lhs f x = isNothing (fmap f x)
rhs f Nothing = True
rhs f (Just _) = False

inspect $ 'lhs === 'rhs
@
-}

-- Description of test obligations

-- | This data type describes an inspection testing obligation.
--
-- It is recommended to build it using 'mkObligation', for backwards
-- compatibility when new fields are added. You can also use the more
-- mnemonic convenience functions like '(===)' or 'hasNoType'.
--
-- The obligation needs to be passed to 'inspect'.
data Obligation = Obligation
    { target      :: Name
        -- ^ The target of a test obligation; invariably the name of a local
        -- definition. To get the name of a function @foo@, write @'foo@. This requires
        -- @{&#45;\# LANGUAGE TemplateHaskell \#&#45;}@.
    , property    :: Property
        -- ^ The property of the target to be checked.
    , testName :: Maybe String
        -- ^ An optional name for the test
    , expectFail  :: Bool
        -- ^ Do we expect this property to fail?
        -- (Only used by 'inspect', not by 'inspectTest')
    , srcLoc :: Maybe Loc
        -- ^ The source location where this obligation is defined.
        -- This is filled in by 'inspect'.
    , storeResult :: Maybe String
        -- ^ If this is 'Nothing', then report errors during compilation.
        -- Otherwise, update the top-level definition with this name.
    }
    deriving Data

-- | Properties of the obligation target to be checked.
data Property
    -- | Are the two functions equal?
    --
    -- More precisely: @f@ is equal to @g@ if either the definition of @f@ is
    -- @f = g@, or the definition of @g@ is @g = f@, or if the definitions are
    -- @f = e@ and @g = e@.
    --
    -- In general @f@ and @g@ need to be defined in this module, so that their
    -- actual defintions can be inspected.
    --
    -- If the boolean flag is true, then ignore types during the comparison.
    = EqualTo Name Bool

    -- | Do none of these types appear anywhere in the definition of the function
    -- (neither locally bound nor passed as arguments)
    | NoTypes [Name]

    -- | Does this function perform no heap allocations.
    | NoAllocation

    -- | Does this value contain dictionaries (/except/ of the listed classes).
    | NoTypeClasses [Name]
    deriving Data

-- | Creates an inspection obligation for the given function name
-- with default values for the optional fields.
mkObligation :: Name -> Property -> Obligation
mkObligation target prop = Obligation
    { target = target
    , property = prop
    , testName = Nothing
    , srcLoc = Nothing
    , expectFail = False
    , storeResult = Nothing
    }

{- $convenience

These convenience functions create common test obligations directly.
-}

-- | Declare two functions to be equal (see 'EqualTo')
(===) :: Name -> Name -> Obligation
(===) = mkEquality False False
infix 9 ===

-- | Declare two functions to be equal, but ignoring
-- type lambdas, type arguments and type casts (see 'EqualTo')
(==-) :: Name -> Name -> Obligation
(==-) = mkEquality False True
infix 9 ==-

-- | Reclare two functions to be equal, but expect the test to fail (see 'EqualTo' and 'expectFail')
-- (This is useful for documentation purposes, or as a TODO list.)
(=/=) :: Name -> Name -> Obligation
(=/=) = mkEquality True False
infix 9 =/=

mkEquality :: Bool -> Bool -> Name -> Name -> Obligation
mkEquality expectFail ignore_types n1 n2 =
    (mkObligation n1 (EqualTo n2 ignore_types))
        { expectFail = expectFail }

-- | Declare that in a function’s implementation, the given type does not occur.
--
-- More precisely: No locally bound variable (let-bound, lambda-bound or
-- pattern-bound) has a type that contains the given type constructor.
--
-- @'inspect' $ fusedFunction ``hasNoType`` ''[]@
hasNoType :: Name -> Name -> Obligation
hasNoType n tn = mkObligation n (NoTypes [tn])

-- | Declare that a function’s implementation does not contain any generic types.
-- This is just 'asNoType' applied to the usual type constructors used in
-- "GHC.Generics".
--
-- @inspect $ hasNoGenerics genericFunction@
hasNoGenerics :: Name -> Obligation
hasNoGenerics n =
    mkObligation n
        (NoTypes [ ''V1, ''U1, ''M1, ''K1, ''(:+:), ''(:*:), ''(:.:), ''Rec1
                 , ''Par1
                 ])

-- | Declare that a function's implementation does not include dictionaries.
--
-- More precisely: No locally bound variable (let-bound, lambda-bound or
-- pattern-bound) has a type that contains a type that mentions a type class.
--
-- @'inspect' $ 'hasNoTypeClasses' specializedFunction@
hasNoTypeClasses :: Name -> Obligation
hasNoTypeClasses n = hasNoTypeClassesExcept n []

-- | A variant of 'hasNoTypeClasses', which white-lists some type-classes.
--
-- @'inspect' $ fieldLens ``hasNoTypeClassesExcept`` [''Functor]@
hasNoTypeClassesExcept :: Name -> [Name] -> Obligation
hasNoTypeClassesExcept n tns = mkObligation n (NoTypeClasses tns)

-- | Internal class that prevents compilation when the plugin is not loaded
class PluginNotLoaded

_pretendItsUsed :: PluginNotLoaded => ()
_pretendItsUsed = ()

needsPluginQ :: Q [Dec]
#if MIN_VERSION_GLASGOW_HASKELL(8,5,0,0)
needsPluginQ = getQ >>= \case
    Just NeedsPluginInserted -> return []
    Nothing -> do
        putQ NeedsPluginInserted
        [d| needsTestInspectionPlugin :: ()
            needsTestInspectionPlugin = (() :: PluginNotLoaded => ())
            |]

-- | To ensure we insert needsPlugin only once
data NeedsPluginInserted = NeedsPluginInserted
#else
needsPluginQ = return []
#endif

-- The exported TH functions

inspectCommon :: AnnTarget -> Obligation -> Q [Dec]
inspectCommon annTarget obl = do
    loc <- location
    annExpr <- liftData (obl { srcLoc = Just loc })
    np <- needsPluginQ
    pure $ np ++ [PragmaD (AnnP annTarget annExpr)]

-- | As seen in the example above, the entry point to inspection testing is the
-- 'inspect' function, to which you pass an 'Obligation'.
-- It will report test failures at compile time.
inspect :: Obligation -> Q [Dec]
inspect = inspectCommon ModuleAnnotation

-- | The result of 'inspectTest', which has a more or less helpful text message
data Result = Failure String | Success String
    deriving Show

didNotRunPluginError :: Result
didNotRunPluginError = lazy (error "Test.Inspection.Plugin did not run")
{-# NOINLINE didNotRunPluginError #-}

-- | This is a variant that allows compilation to succeed in any case,
-- and instead indicates the result as a value of type 'Result',
-- which allows seamless integration into test frameworks.
--
-- This variant ignores the 'expectFail' field of the obligation. Instead,
-- it is expected that you use the corresponding functionality in your test
-- framework (e.g. tasty-expected-failure)
inspectTest :: Obligation -> Q Exp
inspectTest obl = do
    nameS <- genName
    name <- newUniqueName nameS
    anns <- inspectCommon (ValueAnnotation name) obl
    addTopDecls $
        [ SigD name (ConT ''Result)
        , ValD (VarP name) (NormalB (VarE 'didNotRunPluginError)) []
        , PragmaD (InlineP name NoInline FunLike AllPhases)
        ] ++ anns
    return $ VarE name
  where
    genName = do
        (r,c) <- loc_start <$> location
        return $ "inspect_" ++ show r ++ "_" ++ show c

-- | Like newName, but even more unique (unique across different splices),
-- and with unique @nameBase@s. Precondition: the string is a valid Haskell
-- alphanumeric identifier (could be upper- or lower-case).
newUniqueName :: Quasi q => String -> q Name
newUniqueName str = do
  n <- qNewName str
  qNewName $ show n
-- This is from https://ghc.haskell.org/trac/ghc/ticket/13054#comment:1
