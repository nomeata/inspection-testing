module GHC.Proof where

proof :: a -> a -> ()
proof _ _ = ()
{-# INLINE [0] proof #-}

(===) :: a -> a -> ()
(===) = proof
infix 0 ===

non_proof :: a -> a -> ()
non_proof _ _ = ()
{-# INLINE [0] non_proof #-}

(=/=) :: a -> a -> ()
(=/=) = non_proof
infix 0 =/=


