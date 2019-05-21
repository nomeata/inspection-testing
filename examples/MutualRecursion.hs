{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
module MutualRecursion where

import System.Exit
import Test.Inspection

inf = go0
  where
    go0 = 'a' : go1
    go1 = 'b' : go2
    go2 = 'c' : go0
inf' = go0
  where
    go1 = 'b' : go2
    go0 = 'a' : go1
    go2 = 'c' : go0
inf2' = go0
  where
    go1 = 'b' : go2
    go2 = 'c' : go0
    go0 = 'a' : go1
inf3' = go0
  where
    go0 = 'a' : go1
    go1 = 'b' : go2
    go2 = 'c' : go0

inspect $ 'inf === 'inf'
inspect $ 'inf === 'inf2'
inspect $ 'inf === 'inf3'

letrec =
  let go0 = 'a' : go1
      go1 = 'b' : go2
      go2 = 'c' : go0
   in go0
letrec' =
  let go1 = 'b' : go2
      go0 = 'a' : go1
      go2 = 'c' : go0
   in go0
letrec2' =
  let go1 = 'b' : go2
      go2 = 'c' : go0
      go0 = 'a' : go1
   in go0
letrec3' =
  let go0 = 'a' : go1
      go1 = 'b' : go2
      go2 = 'c' : go0
   in go0

inspect $ 'letrec === 'letrec'
inspect $ 'letrec === 'letrec2'
inspect $ 'letrec === 'letrec3'

something_else =
  let go0 = 'a' : go1
      go1 = 'b' : go0
      go2 = 'c' : go0
   in go0

isSuccess :: Result -> Bool
isSuccess (Success _) = True
isSuccess (Failure _) = False

main :: IO ()
main = do
  if isSuccess $(inspectTest $ 'letrec === 'something_else)
     then exitFailure  -- these things are not equal
     else exitSuccess
