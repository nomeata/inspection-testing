Inspection Testing for Haskell
==============================

This GHC plugin allows you to embed assertions about the intermediate code into
your Haskell code, and have them checked by GHC. This is called _inspection
testing_ (as it automates what you do when you manually inspect the
intermediate code).

Synopsis
--------

See the `Test.Inspection` module for the documentation, but there really isn't much
more to it than:

```haskell
{-# LANGAUGE TemplateHaskell #-}
{-# OPTIONS_GHC -O -fplugin Test.Inspection.Plugin #-}
module Simple where

import Test.Inspection
import Data.Maybe

lhs, rhs :: (a -> b) -> Maybe a -> Bool
lhs f x = isNothing (fmap f x)
rhs f Nothing = True
rhs f (Just _) = False

inspect $ 'lhs === 'rhs
```

If you compile this, you will reassurringly read:

```
$ ghc Simple.hs
[1 of 1] Compiling Simple           ( Simple.hs, Simple.o )
examples/Simple.hs:14:1: inspecting lhs === rhs
Test.Inspection tested 1 obligation
```

See the [`examples/`](examples/) directory for more examples of working proofs.

If an assertion fails, for example

```haskell
bad1, bad2 :: Int
bad1 = 2 + 2
bad2 = 5

inspect $ 'bad1 === 'bad2
```
then the compiler will tell you so, and abort the compilation:
```
$ ghc Simple.hs
[1 of 1] Compiling Simple           ( Simple.hs, Simple.o )
examples/Simple.hs:20:1: inspecting bad1 === bad2
Obligation fails
    LHS: ghc-prim-0.5.1.0:GHC.Types.I# 4#
    RHS: ghc-prim-0.5.1.0:GHC.Types.I# 5#
examples/Simple.hs: error: inspection testing unsuccessful
```


Can I comment or help?
----------------------

Sure! We can use the GitHub issue tracker for discussions, and obviously
contributions are welcome.

