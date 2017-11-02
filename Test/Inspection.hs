-- |
-- Description : Let GHC prove program equations
-- Copyright   : (c) Joachim Breitner, 2017
-- License     : MIT
-- Maintainer  : mail@joachim-breitner.de
-- Portability : GHC specifc
--
-- This module supports the accompanying GHC plugin "Test.Inspection.Plugin" and adds
-- to GHC the ability to verify simple program equations.
--
-- = Synopis
--
-- Consider this module:
--
-- > {-# OPTIONS_GHC -O -fplugin Test.Inspection.Plugin #-}
-- > module Simple where
-- >
-- > import Test.Inspection
-- > import Data.Maybe
-- >
-- > my_proof1 :: (a -> b) -> Maybe a -> Proof
-- > my_proof1 f x = isNothing (fmap f x)
-- >             === isNothing x
-- >
-- > my_proof2 :: a -> Maybe a -> Proof
-- > my_proof2 d x = fromMaybe d x
-- >             === maybe d id x
--
-- Compiling it will result in this output:
--
-- > $ ghc Simple.hs
-- > [1 of 1] Compiling Simple           ( Simple.hs, Simple.o )
-- > Test.Inspection: Proving my_proof1 …
-- > Test.Inspection: Proving my_proof2 …
-- > Test.Inspection proved 2 equalities
--
-- = Usage
--
-- To use this plugin, you have to
--
-- *   Make sure you load the plugin @Test.Inspection.Plugin@, either by passing
--     @-fplugin Test.Inspection.Plugin@ to GHC or, more conveniently, using the
--     @OPTIONS_GHC@ pragma as above.
--
-- *   Import the @Test.Inspection@ module.
--
-- *   Define proof obligations using the 'proof' function or, equilvalently, the
--     '===' operator. Type signatures are optional.
--
--     These proof obligation must occur direclty on the
--     right-hand side of a top-level definition, where all parameters (if any)
--     are plain variables. For example, this would (currently) not work:
--
--     > not_good (f,x) = isNothing (fmap f x) === isNothing x
--
--     If your module has an explicit export list, then these functions need to
--     be exported (otherwise the compiler deletes them too quickly).
--
-- *   Compile. If all proof obligations can be proven, compilation continues as
--     usual; otherwise it aborts.
--
-- = What can I prove this way?
--
-- Who knows... but generally you can only expect interesting results when you
-- use functions that are either non-recursive, or have an extensive rewrite
-- rule setup (such as lists). See the @examples/@ directory for some examples
-- of what works.
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
module Test.Inspection ((===), (=/=)) where

import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (getQ, putQ)

import Data.Maybe (fromMaybe)

-- Some internal names, to be used to mark the obligations

proof :: a -> a -> ()
proof _ _ = ()
{-# NOINLINE proof #-}

non_proof :: a -> a -> ()
non_proof _ _ = ()
{-# NOINLINE non_proof #-}

keep_alive :: a -> a
keep_alive = id
{-# NOINLINE keep_alive #-}

-- The exported TH functions

newtype Cntr = Cntr Int

oblName :: Q Name
oblName = do
    Cntr n <- fromMaybe (Cntr 0) <$> getQ
    let !m = n + 1
    putQ (Cntr m)
    newName $ "obligation" ++ show m

(===) :: Name -> Name -> Q [Dec]
n1 === n2 = do
    n <- oblName
    def <-   [d| $(varP n) = proof $(varE n1) $(varE n2) |]
    let p1 = [PragmaD (InlineP n NoInline FunLike AllPhases)]
    p2 <-    [d| {-# RULES "keep-alive" keep_alive $(varE n) = $(varE n) #-} |]
    return (def ++ p1 ++ p2)
infix 0 ===

(=/=) :: Name -> Name -> Q [Dec]
n1 =/= n2 = do
    n <- oblName
    def <-   [d| $(varP n) = non_proof $(varE n1) $(varE n2) |]
    let p1 = [PragmaD (InlineP n NoInline FunLike AllPhases)]
    p2 <-    [d| {-# RULES "keep-alive" keep_alive $(varE n) = $(varE n) #-} |]
    return (def ++ p1 ++ p2)
infix 0 =/=
