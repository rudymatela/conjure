Changelog for (Code) Conjure
============================


v0.6.6 (February 2025)
----------------------

* (pruning) test candidate cases earlier and indepentently
  (can be disabled with `earlyTests=False`);
* (pruning) rewrite after filling in recursions;
* improve showing of test values;
* some internal refactoring and code cleanup;
* update documentation of new interfaces
  with runtimes in examples
  and removal of uneeded uses of maxSize.


v0.6.4 (February 2025)
----------------------

* Create `Args.target` as the main setting to configure
  a target number of candidates to explore.
  This should relate more closely with runtime.
  `target = 10080` is the default.
* relax `maxSize=12` to `maxSize=24` by default.
* show runtime in the output by default
* some internal refactoring


v0.6.2 (February 2025)
----------------------

* don't require 0 as a base case by default
  (defalt to `requireZero=False`)
* add switch to limit the size of constant sub-expressions
  (`Args.maxConstantSize`)
* add switch to enable showing of allowed patterns
  (`Args.showPatterns`)
* update examples


v0.6.0 (February 2025)
----------------------

* `Args` record: add `showCandidates`, `showTests` and `showDeconstructions`
  to make it easier to see what Conjure is doing.
* require `0` as a base case when recursing over `Num`s,
  can be disabled by setting `requireZero=False` on `Args`.
* `conjpure*`: return a record rather than a tuple.
* improve main documentation and introductory examples in Haddock and README.
* improve Haddock documentation a bit throughout
  (`conjureIsDeconstruction`, `deriveConjurable`, etc).
* slightly improve examples and benchmarks
* (internal) improve debugging mechanisms of `Defn` evaluation functions.


v0.5.16 (January 2025)
----------------------

* slightly improve search space pruning
* slightly cleanup code
* fix build of tests on GHC >= 9.10
* improve tests and CI scripts


v0.5.14 (February 2024)
-----------------------

* improve commutative pruning, slightly faster Conjure
* add `carryOn`, `rewriting`, `requireDescent`, `adHocRedundancy` switches
  to the `Args` record
* add more benchmarks and tests


v0.5.12 (February 2024)
-----------------------

* bump Speculate requirement to v0.4.20
* improve testing of Conjure itself
* improvements and fixes of Conjure's benchmarks


v0.5.10 (February 2024)
-----------------------

* improve pruning of candidate functions;
* `Conjure`: un-export experimental `equalModuloTesting`
* `Conjure.Conjurable`: add `conjureListFor`, `conjureSizeFor` & `conjureGrounds`;
* Reorganize internal modules
* Add `Conjure.Defn.Redundancy` module for redundant candidates;
* Add `Conjure.Defn.Test` module testing candidate definitions;
* Add `Conjure.Reason` module for term-rewriting-based reasoning;
* Add `Conjure.Red` module for recursive descent;
* `Conjure.Expr`: add a few auxiliary functions
* Move functions out of `Conjure.Engine` into new modules
* add more examples and benchmarks;
* improved testing of Conjure itself;
* and several other improvements and fixes.


v0.5.8 (January 2024)
---------------------

* prune redundant mutants using a few new rules
* rework numeric recursion criteria
* improve pretty-printing
* improve error handling
* refactor and lint throughout
* Conjurable tuples up to 12-tuples
* bump Express requirement to v1.0.14 (bugfix)


v0.5.6 (November 2023)
----------------------

* `Conjure` module: no main API changes
* `Conjure.Engine`: add `equalModuloTesting`
* `Conjure.Utils`: add some misc functions
* add `bench/redundants` that reports groups of redundant candidates


v0.5.4 (November 2023)
----------------------

This has been the "dev" version after v0.5.2
for almost a couple of years:

* report invalid theories-from-testing
* weed-out some redundant candidates:
	- add and use `isRedundantDefn`
	- update how deconstructions are handled
* add (but not use) `conjureSize` to `Conjurable`


v0.5.2 (March 2022)
-------------------

* show number of tested candidates
* complete `Conjurable` derivation functions
* reference related work on README
* add switch to unique-modulo-testing candidates (slow)
  to allow computing the near upper/lower limit on pruning


v0.5.0 (September 2021)
-----------------------

* allow synthesizing/conjuring from properties with `conjureFromSpec`;
* complete Haddock documentation;
* remove several unused functions;
* add stub `conjurableDerive` functions;
* Makefile: add targets to run GPS(2) and TerpreT benches.


v0.4.4 (September 2021)
-----------------------

* remove need for explicit deconstructions:
	- use `-` and `1` instead of `dec`;
	- allow `mod` and `div` as deconstructions;
* bump Express requirement to v1.0.6 (bugfix);
* complete the GPS1 benchmark;
* add GPS2 and TerpreT benchmarks;
* minor fixes in the README.


v0.4.2 (August 2021)
--------------------

* default to using top-level patterns on generated functions;
* memoize function evaluation;
* double-check theory at the end and report warning on incorrect properties;
* add `prif` to `Conjure`;
* simplify deconstructor discovery and add `conjureSize` to `Conjurable`;
* add `cevaluate`, `ceval` and `cvl` to `Conjure.Conjurable`;
* add `bench/gps` and `bench/lowtests`;
* improve tests and benchmarks.


v0.4.0 (July 2021)
------------------

* background primitives are now provided with `pr` and `prim`.
* report number of rules used in pruning
* require Express v1.0.4 and Speculate v0.4.12
* allow `..` notation
* add benchmarks, replicate, subset, p12, p30 and candidates
* add and use the `Defn` type and `conjureDefns`
* minor changes in benchmarks
* cleanup unused code


v0.3.6 (June 2021)
------------------

* add switch for descending recursions
  to allow generation of `gcd`
* refactor recursion generation (replace a hole later)
* change `conjpureWith` to take `Args`
* rename two args fields to `maxBodyRecursions` and `maxEvalRecursions`
  at this point, the old names were misnomers.


v0.3.4 (June 2021)
------------------

* reallow recursions under `&&` and `||`
  (simplifies the generated `or`, `and`, `set` and `elem` functions)
* only require deconstructions on a non-empty subset of arguments
  (allows `fib01` to be produced)
* limit number of terminal evaluations in `recursiveToDynamic`
* fix bug in `recursiveToDynamic` (not counting some recursions)
* add 4 new benchmarks: `count`, `gcd`, `tree` and `setelem`


v0.3.2 (June 2021)
------------------

* significant runtime reduction in several benchmarks, e.g.:
	- take is now reachable in about 5 seconds
* improved candidate generation:
	- faster runtime
	- fewer redundant/invalid candidates
* limit recursive calls to use deconstructors
	- test to find deconstructors automatically
* improve recursion evaluation method (`revaluate` replaces `recursexpr`)
* add fibonacci benchmark
* minor:
	- record runtimes with one decimal place instead of two
	- add longshot benchmark
	- add intercalate to the list benchmark
	- add stub `Conjure.Constructors` module


v0.3.0 (May 2021)
-----------------

* only automatically include an `if` for the return type of the given function
* add the `take-drop` benchmark
* make bottom-up enumeration more type directed


v0.2.8 (May 2021)
-----------------

* export the `A`, `B`, `C`, `D`, `E` and `F` helper types


v0.2.6 (May 2021)
-----------------

* require Express v0.1.10 due to `hasHole` being now exported there
* require Eq result on `conjure1`, `conjure2` and `conjure3`
* code cleanup and more tests


v0.2.4 (May 2021)
-----------------

* allow conjuring from specifications in addition to partial definitions
  (`conjure1`, `conjure2`, `conjure3` and related functions)
* improve examples
* improve criteria for automatic primitive inclusion:
	- only include `if :: ... -> Bool` if there are `Bool` primitives
	- include `False` and `True` automatically only on Speculate's background
* add code-optional candidate nubbing and debug functions


v0.2.2 (May 2021)
-----------------

* by default, search for 60 argument combinations
  among 100000 enumerated combinations


v0.2.0 (May 2021)
-----------------

* search until 100% match is found and exit
* other misc changes


v0.1.2 (April 2021)
-------------------

For the changelog of earlier versions, check the git commit history.
