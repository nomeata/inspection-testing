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

my_assertion = (\f x -> isNothing (fmap f x))
           === (\f x -> isNothing x)
```

If you compile this, you will reassurringly read:

```
$ ghc Simple.hs
[1 of 1] Compiling Simple           ( Simple.hs, Simple.o )
Test.Inspection: Checking my_assertion …
Test.Inspection checked 1 assertion
```

See the [`examples/`](examples/) directory for more examples of working proofs
(with GHC HEAD).

If an assertion failes, for example

```haskell
not_an_assertion = (2+2::Int) === (5::Int)
```

then the compiler will tell you so, and abort the compilation:
```
$ ghc Simple.hs
[1 of 1] Compiling Simple           ( Simple.hs, Simple.o )
Test.Inspection: Proving not_an_assertion …
Equality doss not hold:
    Simplified LHS: GHC.Types.I# 4#
    Simplified RHS: GHC.Types.I# 5#

Simple.hs: error: Test.Inspection could not check all assertions
```

What is Inspection Testing?
---------------------------


How does it work?
-----------------

GHC is a mighty optimizing compiler, and the centerpiece of optimizing, the
*simplifier* is capable of quite a bit of symbolic execution. We can use this
to prove program equalities, simply by taking two expressions, letting GHC
simplify them as far as possible. If the resulting expressions are the same,
then the original expressions are – as far as the compiler is concerned –
identicial.

The GHC simplifier works on the level of the intermediate language GHC Core,
and failed proofs will be reported as such.

The gory details are as follows: The simplifier is run 8 times, twice for each
of the simplifier phases 4, 3, 2 and 1. In between, the *occurrence analiser*
is run. Near the end, we also run *common-subexpression elimination*.

The simplifier is run with more aggressive flags. In particular, it is
instructed to inline functions aggressively and without worrying about code
size.

Can I comment or help?
----------------------

Sure! We can use the GitHub issue tracker for discussions, and obviously
contributions are welcome.

