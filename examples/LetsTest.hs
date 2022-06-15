{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -dsuppress-all #-}
module Main (main) where

import Test.Inspection
import Data.Char (toUpper)
import System.Exit

lhs1, rhs1 :: Char -> Char -> String

lhs1 x y = let x' = toUpper x
               y' = toUpper y
           in [x', x', y', y']

rhs1 x y = let y' = toUpper y
               x' = toUpper x
           in [x', x', y', y']

-- recursive
lhs2, rhs2, rhs2b :: String
lhs2 = let zs = 'z' : xs
           xs = 'x' : ys
           ys = 'y' : xs
       in zs

rhs2 = let ys = 'y' : xs
           xs = 'x' : ys
           zs = 'z' : xs
       in zs

rhs2b = let ys = 'y' : xs
            xs = 'x' : ys
            zs = 'z' : ys
        in zs

printResult :: Result -> IO ()
printResult (Success s) = putStrLn s
printResult (Failure s) = putStrLn s

isSuccess :: Result -> Bool
isSuccess (Success _) = True
isSuccess (Failure _) = False

results :: [Result]
results =
    [ $(inspectTest $ 'lhs1 ==~ 'rhs1)
    , $(inspectTest $ 'lhs2 ==- 'rhs2) -- here GHC orders let bindings by itself!
    , $(inspectTest $ 'lhs2 =/~ 'rhs2b)
    ]

main :: IO ()
main = do
    mapM_ printResult results
    if map isSuccess results == [True, True, False]
    then exitSuccess
    else exitFailure
