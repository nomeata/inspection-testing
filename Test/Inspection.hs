-- |
-- Description : Inspection Testing for Haskell
-- Copyright   : (c) Joachim Breitner, 2017
-- License     : MIT
-- Maintainer  : mail@joachim-breitner.de
-- Portability : GHC specifc
--
-- This module supports the accompanying GHC plugin "Test.Inspection.Plugin" and adds
-- to GHC the ability to do inspeciton testing.

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
module Test.Inspection (
    -- * Synopsis
    -- $synposis

    -- * Registering obligations
    inspect,
    -- * Defining obligations
    Obligation(..), mkObligation, Property(..), (===), (=/=), hasNoType, ) where

import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (getQ, putQ, liftData)
import Data.Data

import Data.Maybe (fromMaybe)
import qualified Data.Set as S

import Test.Inspection.Internal

{- $synposis

To use inspection testing, you need to

 1. enable the @TemplateHaskell@ langauge extension
 2. load the plugin using @-fplugin Test.Inspection.Plugin@
 3. declare your proof obligations using 'inspect'

An example module is

@
{&#45;\# LANGAUGE TemplateHaskell \#&#45;}
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
    , srcLoc :: Maybe Loc
        -- ^ The source location where this obligation is defined.
        -- This is filled in by 'inspect'.
    }
    deriving Data

-- | Properties of the obligation target to be checked.
data Property
    -- | Are the two functions equal?
    --
    -- More precisely: @f@ is equal to @g@ if either the definition of @f@ is
    -- @f = g@, or the definition of @g@ is @g = f@, or if the definitions are
    -- @f = e@ and @g = e@.
    = EqualTo Name

    -- | Does this type not occur anywhere in the definition of the function
    -- (neither locally bound nor passed as arguments)
    | NoType Name

    -- | Does this function perform no heap allocations.
    | NoAllocation
    deriving Data

allLocalNames :: Obligation -> [Name]
allLocalNames obl = target obl : goProp (property obl)
  where
    goProp :: Property -> [Name]
    goProp (EqualTo n) = [n]
    goProp _ = []

-- | Creates an inspection obligation for the given function name
-- with default values for the optional fields.
mkObligation :: Name -> Property -> Obligation
mkObligation target prop = Obligation
    { target = target
    , property = prop
    , testName = Nothing
    , srcLoc = Nothing
    , expectFail = False
    }

-- | Convenience function to declare two functions to be equal
(===) :: Name -> Name -> Obligation
(===) = mkEquality False
infix 1 ===

-- | Convenience function to declare two functions to be equal, but expect the test to fail
-- (This is useful for documentation purposes, or as a TODO list.)
(=/=) :: Name -> Name -> Obligation
(=/=) = mkEquality True
infix 1 =/=

mkEquality :: Bool -> Name -> Name -> Obligation
mkEquality expectFail n1 n2 = (mkObligation n1 (EqualTo n2)) { expectFail = expectFail }

-- | Convenience function to declare that a function’s implementation does not
-- mention a type
--
-- @inspect $ fusedFunction `hasNoType` ''[]@
hasNoType :: Name -> Name -> Obligation
hasNoType n tn = mkObligation n (NoType tn)

-- The exported TH functions

-- | As seen in the example above, the entry point to inspection testing is the
-- 'inspect' function, to which you pass an 'Obligation'.
inspect :: Obligation -> Q [Dec]
inspect obl = do
    loc <- location
    annExpr <- liftData (obl { srcLoc = Just loc })
    rememberDs <- concat <$> mapM rememberName (allLocalNames obl)
    pure $ PragmaD (AnnP ModuleAnnotation annExpr) : rememberDs

-- We need to ensure that names refernced in obligations are kept alive
-- We do so by annotating them with 'KeepAlive'

newtype SeenNames = SeenNames (S.Set Name)

-- Annotate each name only once
nameSeen :: Name -> Q Bool
nameSeen n = do
    SeenNames s <- fromMaybe (SeenNames S.empty) <$> getQ
    let seen = n `S.member` s
    unless seen $ putQ $ SeenNames (S.insert n s)
    pure seen

rememberName :: Name -> Q [Dec]
rememberName n = do
    seen <- nameSeen n
    if seen then return [] else do
        kaExpr <- liftData KeepAlive
        pure [ PragmaD (AnnP (ValueAnnotation n) kaExpr) ]
