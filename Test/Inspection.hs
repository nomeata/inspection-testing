-- |
-- Description : Inspection Testing for Haskell
-- Copyright   : (c) Joachim Breitner, 2017
-- License     : MIT
-- Maintainer  : mail@joachim-breitner.de
-- Portability : GHC specifc
--
-- This module supports the accompanying GHC plugin "Test.Inspection.Plugin" and adds
-- to GHC the ability to do inspeciton testing.
--
-- TODO: Write this documentation. For now, see the READE.md

{-# LANGUAGE TemplateHaskell #-}
module Test.Inspection ((===), (=/=), Obligation(..), THName(..)) where

import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (getQ, putQ, liftData)
import Data.Data

import Data.Maybe (fromMaybe)

import Debug.Trace

import Test.Inspection.Internal

-- The exported TH functions

newtype Cntr = Cntr Int

{-
oblName :: Q String
oblName = do
    Cntr n <- fromMaybe (Cntr 0) <$> getQ
    let !m = n + 1
    putQ (Cntr m)
    pure $ "obligation" ++ show m
-}

rememberName :: Name -> Q [Dec]
rememberName n = do
    let thName = THName n
    thNameExpr <- liftData thName
    -- lhs <- [| keep_alive |]
    -- rhs <- [| $(varE n) |]
    pure [ PragmaD (AnnP (ValueAnnotation n) thNameExpr)

         -- Unclear if this is required to keep the name alive,
         -- or if the annotation is enough. It looks like it...
         -- , PragmaD (RuleP ("keepAlive/"++nameBase n) [] lhs rhs AllPhases)
         ]

-- | This splice asserts that the two referenced functions are equal
-- after compilation.
(===) :: Name -> Name -> Q [Dec]
(===) = annotateEquality True
infix 0 ===

-- | This splice asserts that one wants the two referenced functions
-- to be equal after compilation, but the compiler is not up to it yet.
-- (This is useful for documentation purposes, or as a TODO list.)
(=/=) :: Name -> Name -> Q [Dec]
(=/=) = annotateEquality False
infix 0 =/=

annotateEquality :: Bool -> Name -> Name -> Q [Dec]
annotateEquality really n1 n2 = do
    let ann = Equal really n1 n2
    annExpr <- liftData ann

    -- n <- oblName
    concat <$> (sequence
        [ rememberName n1
        , rememberName n2
        , pure [PragmaD (AnnP ModuleAnnotation annExpr)]
        ])
{-
    lhs <- [| obligation  |]
    rhs <- [| proof $(varE n1) $(varE n2) |]
    pure [PragmaD (RuleP n [] lhs rhs AllPhases)]
-}
