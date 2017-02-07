Prove program equations with GHC
================================

This GHC plugin allows you to embed code equation into your code, and have them
checked by GHC.

Synopsis
--------

See the `GHC.Proof` module for the documentation, but there really isn't much
more to it than:

```haskell
{-# OPTIONS_GHC -O -fplugin GHC.Proof.Plugin #-}
module Simple where

import GHC.Proof
import Data.Maybe

my_proof1 = (\f x -> isNothing (fmap f x))
        === (\f x -> isNothing x)
```

If you compile this, you will reassurringly read:

```
$ ghc Simple.hs
[1 of 1] Compiling Simple           ( Simple.hs, Simple.o )
GHC.Proof: Proving my_proof1 …
GHC.Proof proved 1 equalities
```

See the [`examples/`](examples/) directory for more examples of working proofs
(with GHC HEAD).

If you have proof that GHC cannot prove, for example

```haskell
not_a_proof = (2+2::Int) === (5::Int)
```

then the compiler will tell you so, and abort the compilation:
```
$ ghc Simple.hs
[1 of 1] Compiling Simple           ( Simple.hs, Simple.o )
GHC.Proof: Proving not_a_proof …
Proof failed
    Simplified LHS: GHC.Types.I# 4#
    Simplified RHS: GHC.Types.I# 5#
    Differences:
    • 4# /= 5#

Simple.hs: error: GHC.Proof could not prove all equalities
```

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


Why is this so great?
---------------------

 * You can annotate your code with proofs, in the same file, in the same
   language, without extra tools (besides the plugin).
 * The proofs stay with the code and are run with every compilation.
 * The proof goes through when *the compiler* thinks the expressions are the
   same. There is no worry about whether an external proof tools captures the
   semantics of GHC’s Haskell precisely.
 * It suports, in principle, all of Haskell’s syntax, including a huge number
   of extensions.
 * It is super easy (if it works).
 * Using rewrite rules allows proofs with regard to some theory (e.g. with
   regard to equations about `foldr` and other list combinators), independent
   of whether these are proven.

Why is this not so great?
--------------------

 * It can only prove quite simple things right now.
 * Even for easy things, the proof might fail because GHC simply simplifies the
   expressions slightly different, and there is not always an easy way of
   fixing this.
 * The proofs depend on optimization flags.
 * There is no guarantee that the next GHC release will be able to prove the
   same things.
 * Failed proofs are reported in GHC Core instead of Haskell.
 * At least currently, it more or less requires GHC HEAD (it compiles with
   GHC-8.0, but it has less control over the simplifier and less proofs will go
   through.)

What can it prove?
------------------

Not everything. By far.

But some nice, practical results work, see for example the
[proofs of the `Applicative` and `Monad` laws for `Control.Applicative.Succs`](examples/Successors.hs).

Best results were observed with compositions of non-recursive
functions that handle non-recursive data or handle lists usind standard list
combinators.

The GHC simplifier generally refuses to inline recursive functions, so there is
not much we can do with these for now.

My proof is not found. What can I do?
-------------------------------------

The plugin searches for top-level bindings of the form
```haskell
somename = proof expression1 expression2
```
or
```haskell
somename = expression1 === expression2
```

GHC will drop them before the plugin sees them, though, if `somename` is not
exported, so make sure it is exported. If you really do not want to export it, then you can keep it alive using the trick of
```haskell
{-# RULES "keep somename alive" id somename = somename #-}
```

If it still does not work, check the output of `-dverbose-core2core` for why
your binding does not have the expected form. Maybe you can fix it somehow.

My proof does not go through. Can I fix that?
---------------------------------------------

Maybe. Here are some tricks that sometimes help:

 *  Check if your functions are properly unfolded in the proof. Maybe an
    `INLINEABLE` pragma helps.

 *  Use `{-# LANGUAGE BangPatterns #-}` and mark some arguments as strict:

    ```haskell
    my_proof2 = (\d !x -> fromMaybe d x)
            === (\d !x -> maybe d id x)
    ```

    GHC makes these functions strict by putting the body in a case. This has
    roughly the same effect as s *case split* in interactive theorem proving.

 *  Allow GHC to assume one of your functions is strict:

    ```haskell
    str :: (a -> b) -> (a -> b)
    str f x = x `seq` f x

    monad_law_3 = (\ (x::Succs a) k h -> x >>= (\x -> k x >>= str h))
              === (\ (x::Succs a) k h -> (x >>= k) >>= str h)
    ```

 *  Instead of using recursion, try to use combinators (e.g. `filter`, `map`, `++` etc.).

 *  Add rewrite rules to tell GHC about some program equations that it should
    use while simplifying. This is in particular useful when working with list functions

    Here are some examples:
    ```haskell
    {-# RULES "mapFB/id" forall c . mapFB c (\x -> x) = c #-}
    {-# RULES "foldr/nil" forall k n . GHC.Base.foldr k n [] = n #-}
    {-# RULES "foldr/mapFB" forall c f g n1 n2 xs.
        GHC.Base.foldr (mapFB c f) n1 (GHC.Base.foldr (mapFB (:) g) n2 xs)
        = GHC.Base.foldr (mapFB c (f.g)) (GHC.Base.foldr (mapFB c f) n1 n2) xs
        #-}
    ```

    But note that these apply to your whole module, and are exported from it, so you
    should not attempt to add such a rule to the same module where you prove
    the rule. And if you don’t want these rules to be applied in normal code,
    put your proofs into a separate `Proof` module that is never imported.


Shall I use this in production?
-------------------------------

You can try. It certainly does not hurt, and proofs that go through are fine.
It might not prove enough to be really useful.

What next?
----------

It remains to be seen how useful this approach really is, and what can be done
to make it more useful. So we need to start proving some things.

Here are some aspects that likely need to be improved:

 * The user should be put into control of some of the simplifier settings.
   Depending on the proof, one might want to go through more or less of the
   simplifier phases, or disable and enable certain rules.

 * Maybe a syntax that does not abuse term-level bindings can be introduced.
   Currently, though, this is not possible for a plugin.

 * If deemed useful, this functionaly maybe can become part of GHC, and the
   simplifier could get a few extra knobs to turn.

 * A custom function to compare expressions that relates more than just
   alpha-equivalence could expand the scope of this plugin.

 * The reporting of failed proofs can be improved.

 * Come up with a better story about recursive functions.


Can I comment or help?
----------------------

Sure! We can use the GitHub issue tracker for discussions, and obviously
contributions are welcome.

