module GHC.Proof where

data Proof = Proof

proof :: a -> a -> Proof
proof _ _ = Proof
{-# INLINE [0] proof #-}

(===) :: a -> a -> Proof
(===) = proof
infix 0 ===

non_proof :: a -> a -> Proof
non_proof _ _ = Proof
{-# INLINE [0] non_proof #-}

(=/=) :: a -> a -> Proof
(=/=) = non_proof
infix 0 =/=


