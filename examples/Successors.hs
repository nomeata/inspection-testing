{-
This example is based on Control.Applicative.Successors, and shows how some
algebraic laws of a simple data structure can be proven by GHC.
-}
{-# LANGUAGE BangPatterns, ScopedTypeVariables, TypeApplications#-}
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

getCurrent_prop1 x = getCurrent (pure x)
                 === x
getCurrent_prop2 f x = (x `seq` getCurrent (f <*> x))
                   === (x `seq` getCurrent f (getCurrent x))
getCurrent_prop3 x k = getCurrent (x >>= k)
                   === (x `seq` getCurrent (k (getCurrent x)))

getSuccs_prop1 x = getSuccs (pure x)
               === []
getSuccs_prop2 f x = (x `seq` getSuccs (f <*> x))
                 === (x `seq` map ($ getCurrent x) (getSuccs f) ++ map (getCurrent f) (getSuccs x))
getSuccs_prop3 x k = getSuccs (x >>= k)
                 === map (getCurrent . k) (getSuccs x) ++ getSuccs (k (getCurrent x))

-- And now to the actual laws

functor_law1 :: Succs a -> Proof
functor_law1 v = fmap id v
                 === v

functor_law2 :: (a -> b) -> (b -> c) -> Succs a -> Proof
functor_law2 f g v = fmap g (fmap f v)
                 === fmap (g . f) v

applicative_law1 :: Succs a -> Proof
applicative_law1 v = pure id <*> v
                 === v

applicative_law2 :: Succs (b -> c) -> Succs (a -> b) -> Succs a -> Proof
applicative_law2 u v w = pure (.) <*> u <*> v <*> w
                     === u <*> (v <*> w)

applicative_law3 :: forall a b. (a -> b) -> a -> Proof
applicative_law3 f x = pure f <*> (pure x :: Succs a)
                   === pure (f x)

applicative_law4 :: Succs (a -> b) -> a -> Proof
applicative_law4 u y = u <*> pure y
                   === pure ($ y) <*> u

monad_law1 :: a -> (a -> Succs b) -> Proof
monad_law1 a k = (k a `seq` return a >>= k)
             === k a

monad_law2 :: Succs a -> Proof
monad_law2 m = m >>= return
           === m

-- A little trick to say: Prove this only for strict f
str :: (a -> b) -> (a -> b)
str f x = x `seq` f x

monad_law3 :: Succs a -> (a -> Succs b) -> (b -> Succs c) -> Proof
monad_law3 m k h = m >>= (\x -> k x >>= str h)
               === (m >>= k) >>= str h

-- The law relating the monad and applicative instances

return_pure x = return @Succs x
            === pure @Succs x

ap_star f x = (x `seq` getCurrent f `seq` f <*> x)
          === (x `seq` getCurrent f `seq` f `ap` x)
