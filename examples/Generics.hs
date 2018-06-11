{-# LANGUAGE RankNTypes, DeriveGeneric, TypeApplications, DataKinds, ExistentialQuantification, TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Test.Inspection.Plugin #-}
module Generics (main) where

import GHC.Generics
import Test.Inspection

data Record = MkRecord
  { fieldA :: Int
  , fieldB :: Bool
  } deriving Generic


myRecord :: Record
myRecord = MkRecord 1 True

genericRep :: Rep Record x
genericRep = from myRecord


roundTripRep :: Record
roundTripRep = to $ from myRecord

main :: IO ()
main = return ()

-- the check
inspect $ hasNoGenerics 'roundTripRep
inspect $ (hasNoGenerics 'genericRep) { expectFail = True }

