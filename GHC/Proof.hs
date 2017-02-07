module GHC.Proof where


proof :: a -> a -> ()
proof _ _ = ()
{-# INLINE proof #-}

(===) :: a -> a -> ()
(===) = proof

