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
{-# LANGUAGE TemplateHaskell #-}
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
examples/Simple.hs:14:1: lhs === rhs passed.
inspection testing successful
      expected successes: 1
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
$ ghc Simple.hs -dsuppress-idinfo
[5 of 5] Compiling Simple           ( examples/Simple.hs, examples/Simple.o )
examples/Simple.hs:14:1: lhs === rhs passed.
examples/Simple.hs:20:1: bad1 === bad2 failed:
    LHS:
        bad1 :: Int
        bad1 = I# 4#

    RHS:
        bad2 :: Int
        bad2 = I# 5#


examples/Simple.hs: error:
    inspection testing unsuccessful
          expected successes: 1
         unexpected failures: 1
```

What can I check for?
---------------------

Currently, inspection-testing supports

 * checking two definitions to be equal (useful in the context of generic
   programming)
 * checking the absence of a certain type (useful in the context of list or
   stream fusion)
 * checking the absence of a a use of certian functions
 * checking the absence of allocation (generally useful)
 * checking the absence of typeclass-overloaded code

In general, the checks need to be placed in the same module as the
checked-definition.

Possible further applications includes

 * checking that all recursive functions are (efficiently called) join-points
 * asserting strictness properties (e.g. in `Data.Map.Strict`)
 * peforming some of these checks only within recursive loops

Let me know if you need any of these, or have further ideas.

Help, I am drowning in Core!
----------------------------

inspection-testing prints the Core more or less like GHC would, and the same
flags can be used to control the level of detail. In particular, you might want
to pass to GHC a selection of the following flags:

    -dsuppress-idinfo -dsuppress-coercions -dsuppress-type-applications
    -dsuppress-module-prefixes -dsuppress-type-signatures -dsuppress-uniques


It does not seem to do anything (on GHC < 8.4)
----------------------------------------------

Add this line to your module:

    {-# OPTIONS_GHC -O -fplugin Test.Inspection.Plugin #-}

Can I comment or help?
----------------------

Sure! We can use the GitHub issue tracker for discussions, and obviously
contributions are welcome.

