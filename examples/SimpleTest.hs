{-# LANGUAGE TemplateHaskell #-}
module Main where

import Test.Inspection
import Data.Maybe
import System.Exit

lhs, rhs, something_else :: (a -> b) -> Maybe a -> Bool

lhs f x = isNothing (fmap f x)

rhs _ Nothing = True
rhs _ (Just _) = False

something_else _ _ = False

printResult :: Result -> IO ()
printResult (Success s) = putStrLn s
printResult (Failure s) = putStrLn s

isSuccess :: Result -> Bool
isSuccess (Success _) = True
isSuccess (Failure _) = False

results :: [Result]
results =
    [ $(inspectTest $ 'lhs === 'rhs)
    , $(inspectTest $ 'lhs === 'something_else)
    ]

main :: IO ()
main = do
    mapM_ printResult results
    if map isSuccess results == [True, False]
    then exitSuccess
    else exitFailure
