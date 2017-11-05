Inspection Testing for Haskell
==============================

This GHC plugin allows you to embed assertions about the intermediate code into
your Haskell code, and have them checked by GHC.

Synopsis
--------

See the `Test.Inspection` module for the documentation, but there really isn't much
more to it than:

```haskell
{-# OPTIONS_GHC -O -fplugin Test.Inspection.Plugin #-}
module Simple where

import Test.Inspection
import Data.Maybe

lhs, rhs :: (a -> b) -> Maybe a -> Bool
lhs f x = isNothing (fmap f x)
rhs f x = isNothing x

'lhs === 'rhs
```

If you compile this, you will reassurringly read:

```
$ ghc Simple.hs
[1 of 1] Compiling Simple           ( Simple.hs, Simple.o )
Test.Inspection:     Checking lhs === rhs
Test.Inspection proved 1 equalities
```

See the [`examples/`](examples/) directory for more examples of working proofs.

If an assertion fails, for example

```haskell
bad1, bad2 :: Int
bad1 = 2 + 2
bad2 = 5
```

then the compiler will tell you so, and abort the compilation:
```
$ ghc Simple.hs
[1 of 1] Compiling Simple           ( Simple.hs, Simple.o )
Test.Inspection:     Checking bad1 === bad2
Obligation fails
    LHS: GHC.Types.I# 4#
    RHS: GHC.Types.I# 5#
Simple.hs: error: Test.Inspection could not check all assertions
```

What is Inspection Testing?
---------------------------


Can I comment or help?
----------------------

Sure! We can use the GitHub issue tracker for discussions, and obviously
contributions are welcome.

