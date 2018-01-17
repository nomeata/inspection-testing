{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -O -fplugin=Test.Inspection.Plugin #-}
module Main where

import Test.Inspection
import Data.Maybe

lhs, rhs, something_else :: (a -> b) -> Maybe a -> Bool

lhs f x = isNothing (fmap f x)

rhs _ Nothing = True
rhs _ (Just _) = False

something_else _ _ = False

printResult :: Result -> IO ()
printResult (Success s) = putStrLn s
printResult (Failure s) = putStrLn s

inspectTest "test1" $ 'lhs === 'rhs
inspectTest "test2" $ 'lhs === 'something_else

main :: IO ()
main = mapM_ printResult
    [ test1
    , test2
    ]
