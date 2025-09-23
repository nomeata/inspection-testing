{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -O -fhpc -fplugin-opt=Test.Inspection.Plugin:quiet #-}
module HPCs where

import Test.Inspection

main :: IO ()
main = return ()

theTwo :: Int
theTwo = 2

anotherTwo :: Int
anotherTwo = 2

theOnePlusOne :: Int
theOnePlusOne = 1 + 1

inspect $ 'theTwo ==- 'theOnePlusOne
inspect $ 'theTwo ==- 'anotherTwo
inspect $ 'theTwo =/= 'theOnePlusOne
inspect $ 'theTwo =/= 'anotherTwo
