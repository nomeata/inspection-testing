{-# LANGUAGE TemplateHaskell, CPP #-}
{-# OPTIONS_GHC -dsuppress-all -funfolding-use-threshold=120 #-}
module Fusion where

import Test.Inspection
import Data.List (foldl', sort)

sumUp1 :: Int -> Bool
sumUp1 n = sum [1..n] > 1000

inspect $ 'sumUp1 `hasNoType` ''[]
inspect $ ('sumUp1 `hasNoType` ''Int) { expectFail = True }
inspect $ mkObligation 'sumUp1 NoAllocation

-- This stopped working in GHC-9.0, because
-- * the > 1000 comparison is floated into the recursive join point (ok)
-- * `sumUp2` is compiled with a worker-wrapper split that does not happen for
--   sumUp1 (hard to fix)
-- so I am disabling this part of the test on GHC-9.0

sumUp2 :: Int -> Bool
sumUp2 n | 1 > n = False
sumUp2 n = go 1 0 > 1000
    where
        go m s | m == n    = s + m
               | otherwise = go (m+1) (s+m)

inspect $ 'sumUp1 === 'sumUp2

-- Example for a non-fusing funtion
sumUpSort :: Int -> Int
sumUpSort n = sum . sort $ [1..n]

inspect $ ('sumUpSort `hasNoType` ''[]) { expectFail = True }

main :: IO ()
main = return ()
