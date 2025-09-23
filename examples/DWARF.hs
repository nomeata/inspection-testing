{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -g #-}
module DWARF where

import Test.Inspection
import Data.Maybe

lhs :: (a -> b) -> Maybe a -> Bool
lhs f x = isNothing (fmap f x)

rhs :: (a -> b) -> Maybe a -> Bool
rhs f Nothing = True
rhs f (Just _) = False

inspect $ 'lhs === 'rhs

main :: IO ()
main = return ()
