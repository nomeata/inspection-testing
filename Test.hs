{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
module Test (foo) where

import Test.Inspection

foo = 1
bar = 1
baz = 2

a f g = map f . map g
b f g = map (f . g)

-- Inspection test

'foo === 'bar
'bar === 'foo
'foo =/= 'baz
'bar =/= 'baz
'a === 'b


