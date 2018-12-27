{-# LANGUAGE GADTs, TypeFamilies, DataKinds, PolyKinds, TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -O -fplugin-opt=Test.Inspection.Plugin:quiet #-}
module NS_NP (main) where

import Test.Inspection

data NS (f :: k -> *) (xs :: [k]) where
  Z :: f x -> NS f (x : xs)
  S :: !(NS f xs) -> NS f (x : xs)

data NP (f :: k -> *) (xs :: [k]) where
  Nil  :: NP f '[]
  (:*) :: f x -> !(NP f xs) -> NP f (x : xs)

newtype I a = I a

from :: Ordering -> NS (NP I) '[ '[], '[], '[] ]
from = \ x -> case x of
  LT -> Z Nil
  EQ -> S (Z Nil)
  GT -> S (S (Z Nil))
{-# INLINE from #-}

to :: NS (NP I) '[ '[], '[], '[] ] -> Ordering
to = \ x -> case x of
  (Z Nil) -> LT
  (S (Z Nil)) -> EQ
  (S (S (Z Nil))) -> GT
  _ -> error "unreachable"
{-# INLINE to #-}

roundtrip :: Ordering -> Ordering
roundtrip = to . from
{-# INLINE roundtrip #-}

roundtrip_id :: Ordering -> Ordering
roundtrip_id x = x

main :: IO ()
main = return ()

inspect $ 'roundtrip === 'roundtrip_id
