{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
module MutualRecursion where

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

inspect $ 'inf === 'inf'

inf2 = go0
  where
    go0 = 'a' : go1
    go1 = 'b' : go2
    go2 = 'c' : go0
inf2' = go0
  where
    go1 = 'b' : go2
    go2 = 'c' : go0
    go0 = 'a' : go1

inspect $ 'inf2 === 'inf2'

inf3 = go0
  where
    go0 = 'a' : go1
    go1 = 'b' : go2
    go2 = 'c' : go0
inf3' = go0
  where
    go0 = 'a' : go1
    go1 = 'b' : go2
    go2 = 'c' : go0

inspect $ 'inf3 === 'inf3'

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

inspect $ 'letrec === 'letrec'

letrec2 =
  let go0 = 'a' : go1
      go1 = 'b' : go2
      go2 = 'c' : go0
   in go0
letrec2' =
  let go1 = 'b' : go2
      go2 = 'c' : go0
      go0 = 'a' : go1
   in go0

inspect $ 'letrec2 === 'letrec2'

letrec3 =
  let go0 = 'a' : go1
      go1 = 'b' : go2
      go2 = 'c' : go0
   in go0
letrec3' =
  let go0 = 'a' : go1
      go1 = 'b' : go2
      go2 = 'c' : go0
   in go0

inspect $ 'letrec3 === 'letrec3'

main = pure ()
