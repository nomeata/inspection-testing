{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
module Test (foo, bar) where

import Test.Inspection

foo = 1
bar = 1
baz = 2

-- Inspection test

'foo === 'bar
'bar === 'foo
'foo =/= 'baz


