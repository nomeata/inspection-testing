{-# LANGUAGE TemplateHaskell #-}
module UnsafeCoerce (main) where

import Test.Inspection
import Unsafe.Coerce
import GHC.Exts

lhs :: Any -> Any
lhs a = unsafeCoerce $ unsafeCoerce a + (1 :: Int)

rhs :: Int -> Int
rhs = (+ 1)

inspect $ 'lhs =/= 'rhs
inspect $ 'lhs ==- 'rhs

main :: IO ()
main = return ()
