{-
This example is based on Control.Applicative.Successors, and shows how some
algebraic laws of a simple data structure can be proven by GHC.
-}
{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
{-# OPTIONS_GHC -O -fplugin GHC.Proof.Plugin #-}
module Successors where

import GHC.Proof
import GHC.Base (build, mapFB, foldr)
import Control.Monad (ap)

-- The original code

data Succs a = Succs a [a]

instance Functor Succs where
    fmap f (Succs o s) = Succs (f o) (map f s)

instance Applicative Succs where
    pure x = Succs x []
    Succs f fs <*> Succs x xs = Succs (f x) (map ($x) fs ++ map f xs)

instance Monad Succs where
    return = pure -- this is strangely necesary
    Succs x xs >>= f = Succs y (map (getCurrent . f) xs ++ ys)
      where Succs y ys = f x

-- Two simple accessor function. The docs come with three identities each
--
-- prop> getCurrent (pure x)  == x
-- prop> getCurrent (f <*> x) == (getCurrent f) (getCurrent x)
-- prop> getCurrent (x >>= k) == getCurrent (k (getCurrent x))
getCurrent :: Succs t -> t
getCurrent (Succs x _) = x

-- prop> getSuccs (pure x)  == []
-- prop> getSuccs (f <*> x) == map ($ getCurrent x) (getSuccs f) ++ map (getCurrent f) (getSuccs x)
-- prop> getSuccs (x >>= k) == map (getCurrent . k) (getSuccs x) ++ getSuccs (k (getCurrent x))
getSuccs :: Succs t -> [t]
getSuccs (Succs _ xs) = xs

-- These identities are easily proven by GHC,
-- at least if one helps by making some of the arguments as strict.

getCurrent_proof1 = (\x -> getCurrent (pure x))
                === (\x -> x)

getCurrent_proof2 = (\ f !x -> getCurrent (f <*> x))
                === (\ f !x -> (getCurrent f) (getCurrent x))

getCurrent_proof3 = (\ !x k -> getCurrent (x >>= k))
                === (\ !x k -> getCurrent (k (getCurrent x)))

getSuccs_proof1 = (\x -> getSuccs (pure x))
              === (\x -> [])

getSuccs_proof2 = (\ f !x -> getSuccs (f <*> x))
              === (\ f !x -> map ($ getCurrent x) (getSuccs f) ++ map (getCurrent f) (getSuccs x))
getSuccs_proof3 = (\ x k -> getSuccs (x >>= k))
              === (\ x k -> map (getCurrent . k) (getSuccs x) ++ getSuccs (k (getCurrent x)))

-- And now to the actual laws

app_law_1 = (\ x -> pure id <*> x)
        === ((\ x -> x) :: Succs a -> Succs a)

app_law_2 = (\ a b (c::Succs a) -> pure (.) <*> a <*> b <*> c)
        === (\ a b c -> a <*> (b <*> c))

app_law_3 = (\ f (x::a) -> pure f <*> (pure x :: Succs a))
        === (\ f (x::a) -> pure (f x))

app_law_4 = (\ f (x::a) -> f <*> (pure x :: Succs a))
        === (\ f (x::a) -> pure ($x) <*> f)

monad_law_1 = (\ (x::a) k -> k x `seq` (return x :: Succs a) >>= k)
          === (\ (x::a) k -> k x)

monad_law_2 = (\ (x::Succs a) -> x >>= return)
          === (\ x -> x)

-- A little trick to say: Prove this only for strict f
str :: (a -> b) -> (a -> b)
str f x = x `seq` f x

monad_law_3 = (\ (x::Succs a) k h -> x >>= (\x -> k x >>= str h))
          === (\ (x::Succs a) k h -> (x >>= k) >>= str h)

-- The law relating the monad and applicative instances

return_pure = (\ (x::a) -> return x :: Succs a)
          === (\ (x::a) -> pure x :: Succs a)

ap_star = (\ f !(x::Succs a) -> getCurrent f `seq` f <*> x)
      === (\ f !(x::Succs a) -> getCurrent f `seq` f `ap` x)
