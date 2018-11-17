# Revision history for inspection-testing

## 0.4.1 -- UNRELEASED

* New obligation `doesNotUse`
* Use the Obligation’s testName in the plugin output.
* In `inspect`, do not override `srcLoc` if already present.

## 0.4 -- 2018-10-12

* Support GHC-8.6
* On GHC-8.4 or newer, `inspect` and `inspectTest` will automatically load the
  plugin.

## 0.3 -- 2018-07-07

* On GHC-8.5 or newer, use of `inspect` or `inspectTest` without actually
  loading the plugin will cause compilation to fail at type-checking time
  (thanks to @adamgundry for the idea)
* Support for `hasNoTypeClass` (thanks to @phadej)
* Support for `hasNoGenerics` (thanks to @isovector)
* No need to keep referenced variables alive using annotations:
  Simply mentioning them in a Template Haskell splice keeps them alive!

## 0.2.0.1 -- 2018-02-02

* Support GHC HEAD (8.5)

## 0.2 -- 2018-01-17

* With `$(inspectTest obligation)` you can now get the result of inspection
  testing at run-time, for integration into your test suite.

## 0.1.2 -- 2017-11-20

* Make `(==-)` a bit more liberal, and look through variable redefinitions that
  only change the type

## 0.1.1.2 -- 2017-11-12

* Hotfix: Do not abort if there are expected failures

## 0.1.1.1 -- 2017-11-12

* Show summary stats
* Pull in less tests, to make inclusion in stackage easier

## 0.1.1 -- 2017-11-09

* More complete output when `(===)` fails
* Variant `(==-)` that ignores types when comparing terms

## 0.1 -- 2017-11-09

* Repackaged as inspection-testing

## 0.1.1  -- 2017-09-05

* Also run simplifier in stage 0

## 0.1  -- 2017-08-26

* Initial release to hackage

## 0  -- 2017-02-06

* Development of ghc-proofs commences
