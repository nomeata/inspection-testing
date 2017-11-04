-- |
-- Description : Let GHC prove program equations
-- Copyright   : (c) Joachim Breitner, 2017
-- License     : MIT
-- Maintainer  : mail@joachim-breitner.de
-- Portability : GHC specifc
--
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
module Test.Inspection.Internal (Obligation(..), THName(..)) where

import Data.Data
import Language.Haskell.TH (Name)

-- | The possible test obligations
data Obligation =
    -- | Are the two functions equal
    Equal
            Bool -- ^ Is this test expected
            Name -- ^ compare this to
            Name -- ^ this
    deriving Data

-- | An annotation to keep names alive and allow us to find them again.
newtype THName = THName Name
    deriving Data

{-
keep_alive :: a
keep_alive = keep_alive
{-# NOINLINE keep_alive #-}
-}
