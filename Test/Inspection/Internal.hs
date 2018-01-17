-- |
-- Description : Inspection testing
-- Copyright   : (c) Joachim Breitner, 2017
-- License     : MIT
-- Maintainer  : mail@joachim-breitner.de
-- Portability : GHC specifc
--
{-# LANGUAGE DeriveDataTypeable #-}
module Test.Inspection.Internal
    ( KeepAlive(..)
    ) where

import Data.Data

-- | An annotation to keep names alive
data KeepAlive = KeepAlive
    deriving Data

{-
keep_alive :: a
keep_alive = keep_alive
{-# NOINLINE keep_alive #-}
-}
