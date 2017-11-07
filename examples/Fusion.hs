{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
module Fusion where

import Test.Inspection
import Data.List (foldl')

sumUp1 :: Int -> Int
sumUp1 n = sum [1..n]

sumUp2 :: Int -> Int
sumUp2 n | 1 > n = 0
sumUp2 n = go 1 0
    where
        go m s | m == n     = (s + m)
               | otherwise = go (m+1) (s+m)

inspect $ 'sumUp1 === 'sumUp2
inspect $ 'sumUp1 `hasNoType` ''[]
inspect $ ('sumUp1 `hasNoType` ''Int) { expectFail = True }
inspect $ mkObligation 'sumUp1 NoAllocation

main :: IO ()
main = return ()
