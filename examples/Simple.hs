{-# OPTIONS_GHC -O -fplugin GHC.Proof.Plugin #-}
module Simple where

import GHC.Proof
import Data.Maybe

my_proof1 :: (a -> b) -> Maybe a -> Proof
my_proof1 f x = isNothing (fmap f x)
            === isNothing x

my_proof2 :: a -> Maybe a -> Proof
my_proof2 d x = fromMaybe d x
            === maybe d id x

