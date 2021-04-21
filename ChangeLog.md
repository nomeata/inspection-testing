# Revision history for inspection-testing

## 0.4.4.0 -- 2020-04-21

* More GHC-9.0 compatibility (thanks @aadaa-fgtaa)

## 0.4.3.0 -- 2020-01-26

* Ignores HPC ticks in `(==-)` (thanks @konn)
* Add `(=/-)` operator (thanks @lysxia)
* Add skip-O0 plugin option (thanks @AndrasKovacs)
* GHC-9.0 compatibility (thanks @konn)
* CI now runs on Github Actions (thanks @phadej)

## 0.4.2.4 -- 2020-01-26

* Now prints the name of the type class on which a test fails, thanks to
  Harendra Kumar
* More examples, thanks to Rafe

## 0.4.2.3 -- 2020-01-26

* Support GHC-8.10, thanks to Ryan Scott via head.hackage for the patch

## 0.4.2.1 -- 2019-06-07

* Bugfix release

## 0.4.2 -- 2019-06-05

* Be less picky if mutually recursive definitions appear in a different order
  in the source
* Add obligation `coreOf`, which succeeds, but lets you dump the core of a
  single symbol (thanks to @phadej)
* Support `-fplugin-opt=Test.Inspection.Plugin:keep-going-O0` (thanks to @phadej)

## 0.4.1.2 -- 2019-02-23

* Do not force recompilation with GHC >= 8.6
* Support `-fplugin-opt=Test.Inspection.Plugin:quiet`

## 0.4.1.1 -- 2018-11-17

* Fix a bug with `doesNotUse` and data constructors

## 0.4.1 -- 2018-11-17

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
