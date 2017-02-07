{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
{-# OPTIONS_GHC -O -fplugin GHC.Proof.Plugin #-}
module Simple where

import GHC.Proof
import Data.Maybe

my_proof1 = (\f x -> isNothing (fmap f x))
        === (\f x -> isNothing x)

my_proof2 = (\d !x -> fromMaybe d x)
        === (\d !x -> maybe d id x)

