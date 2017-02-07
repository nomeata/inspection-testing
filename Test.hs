{-# OPTIONS_GHC -fplugin GHC.Proof.Plugin #-}
module Test (foo, bar, proofs) where

import GHC.Proof

foo = 1
bar = 1

proofs = [proof1,proof2]

proof1 = proof foo foo
proof2 = proof foo bar


