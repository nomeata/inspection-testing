{-# LANGUAGE TemplateHaskell #-}
module Simple where

import Test.Inspection
import Data.Maybe

lhs, rhs :: (a -> b) -> Maybe a -> Bool
lhs f x = isNothing (fmap f x)

rhs f Nothing = True
rhs f (Just _) = False

inspect $ 'lhs === 'rhs

main :: IO ()
main = return ()
