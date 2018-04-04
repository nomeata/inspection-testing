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
module Test.Inspection (
    -- * Synopsis
    -- $synposis

    -- * Registering obligations
    inspect,
    inspectTest,
    Result(..),
    -- * Defining obligations
    Obligation(..), mkObligation, Property(..),
    (===), (==-), (=/=), hasNoType,
) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (liftData, addTopDecls)
import Data.Data
import GHC.Exts (lazy)

{- $synposis

To use inspection testing, you need to

 1. enable the @TemplateHaskell@ langauge extension
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
-- memonic convenience functions like '(===)' or 'hasNoType'.
--
-- The obligation needs to be passed to 'inspect'.
data Obligation = Obligation
    { target      :: Name
        -- ^ The target of a test obligation; invariably the name of a local
        -- definition. To get the name of a function @foo@, write @'foo@. This requires
        -- @{&#45;\# LANGAUGE TemplateHaskell \#&#45;}@.
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
    -- If the boolean flag is true, then ignore types during the comparison.
    = EqualTo Name Bool

    -- | Does this type not occur anywhere in the definition of the function
    -- (neither locally bound nor passed as arguments)
    | NoType Name

    -- | Does this function perform no heap allocations.
    | NoAllocation
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

-- | Convenience function to declare two functions to be equal
(===) :: Name -> Name -> Obligation
(===) = mkEquality False False
infix 9 ===

-- | Convenience function to declare two functions to be equal, but ignoring
-- type lambdas, type arguments and type casts
(==-) :: Name -> Name -> Obligation
(==-) = mkEquality False True
infix 9 ==-

-- | Convenience function to declare two functions to be equal, but expect the test to fail
-- (This is useful for documentation purposes, or as a TODO list.)
(=/=) :: Name -> Name -> Obligation
(=/=) = mkEquality True False
infix 9 =/=

mkEquality :: Bool -> Bool -> Name -> Name -> Obligation
mkEquality expectFail ignore_types n1 n2 =
    (mkObligation n1 (EqualTo n2 ignore_types))
        { expectFail = expectFail }

-- | Convenience function to declare that a function’s implementation does not
-- mention a type
--
-- @inspect $ fusedFunction `hasNoType` ''[]@
hasNoType :: Name -> Name -> Obligation
hasNoType n tn = mkObligation n (NoType tn)

-- The exported TH functions

inspectCommon :: AnnTarget -> Obligation -> Q [Dec]
inspectCommon annTarget obl = do
    loc <- location
    annExpr <- liftData (obl { srcLoc = Just loc })
    pure [PragmaD (AnnP annTarget annExpr)]

-- | As seen in the example above, the entry point to inspection testing is the
-- 'inspect' function, to which you pass an 'Obligation'.
-- It will report test failures at compile time.
inspect :: Obligation -> Q [Dec]
inspect = inspectCommon ModuleAnnotation

-- | The result of 'inspectTest', which a more or less helpful text message
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
    name <- newName nameS
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
