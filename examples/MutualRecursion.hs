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

main = pure ()
