{-# LANGUAGE RankNTypes, DeriveGeneric, TypeApplications, DataKinds, ExistentialQuantification #-}
{-# OPTIONS_GHC -O -fplugin GHC.Proof.Plugin #-}
module GenericLens (fieldALensManual, fieldALensGenericYoneda, proof1) where

import GHC.Generics
import Data.Generics.Product
import GHC.Proof

data Record = MkRecord { fieldA :: Int
                       , fieldB :: Bool
                       } deriving Generic

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

fieldALensManual :: Lens' Record Int
fieldALensManual f (MkRecord a b) = (\a -> MkRecord a b) <$> f a

-- Coyoneda optimization

data Coyoneda f b = forall a. Coyoneda (a -> b) (f a)

instance Functor (Coyoneda f) where
    fmap f (Coyoneda g fa) = Coyoneda (f . g) fa

inj :: Functor f => Coyoneda f a -> f a
inj (Coyoneda f a) = fmap f a

proj :: Functor f => f a -> Coyoneda f a
proj fa = Coyoneda id fa

ravel :: Functor f => ((a -> Coyoneda f b) -> (s -> Coyoneda f t))
                   -> (a -> f b) -> (s -> f t)
ravel coy f s = inj $ coy (\a -> proj (f a)) s

-- the proof


fieldALensGeneric :: Lens' Record Int
fieldALensGeneric = field @"fieldA"

fieldALensGenericYoneda :: Lens' Record Int
fieldALensGenericYoneda = ravel (field @"fieldA")

proof1 f = fieldALensManual f === fieldALensGenericYoneda f

