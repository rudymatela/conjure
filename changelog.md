Changelog for (Code) Conjure
============================


v0.5.0
------

* allow synthesizing/conjuring from properties with `conjureFromSpec`;
* complete Haddock documentation;
* remove several unused functions;
* add stub `conjurableDerive` functions;
* Makefile: add targets to run GPS(2) and TerpreT benches.


v0.4.4
------

* remove need for explicit deconstructions:
	- use `-` and `1` instead of `dec`;
	- allow `mod` and `div` as deconstructions;
* bump Express requirement to v1.0.6 (bugfix);
* complete the GPS1 benchmark;
* add GPS2 and TerpreT benchmarks;
* minor fixes in the README.


v0.4.2
------

* default to using top-level patterns on generated functions;
* memoize function evaluation;
* double-check theory at the end and report warning on incorrect properties;
* add `prif` to `Conjure`;
* simplify deconstructor discovery and add `conjureSize` to `Conjurable`;
* add `cevaluate`, `ceval` and `cvl` to `Conjure.Conjurable`;
* add `bench/gps` and `bench/lowtests`;
* improve tests and benchmarks.


v0.4.0
------

* background primitives are now provided with `pr` and `prim`.
* report number of rules used in pruning
* require Express v1.0.4 and Speculate v0.4.12
* allow `..` notation
* add benchmarks, replicate, subset, p12, p30 and candidates
* add and use the `Defn` type and `conjureDefns`
* minor changes in benchmarks
* cleanup unused code


v0.3.6
------

* add switch for descending recursions
  to allow generation of `gcd`
* refactor recursion generation (replace a hole later)
* change `conjpureWith` to take `Args`
* rename two args fields to `maxBodyRecursions` and `maxEvalRecursions`
  at this point, the old names were misnomers.


v0.3.4
------

* reallow recursions under `&&` and `||`
  (simplifies the generated `or`, `and`, `set` and `elem` functions)
* only require deconstructions on a non-empty subset of arguments
  (allows `fib01` to be produced)
* limit number of terminal evaluations in `recursiveToDynamic`
* fix bug in `recursiveToDynamic` (not counting some recursions)
* add 4 new benchmarks: `count`, `gcd`, `tree` and `setelem`


v0.3.2
------

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


v0.3.0
------

* only automatically include an `if` for the return type of the given function
* add the `take-drop` benchmark
* make bottom-up enumeration more type directed


v0.2.8
------

* export the `A`, `B`, `C`, `D`, `E` and `F` helper types


v0.2.6
------

* require Express v0.1.10 due to `hasHole` being now exported there
* require Eq result on `conjure1`, `conjure2` and `conjure3`
* code cleanup and more tests


v0.2.4
------

* allow conjuring from specifications in addition to partial definitions
  (`conjure1`, `conjure2`, `conjure3` and related functions)
* improve examples
* improve criteria for automatic primitive inclusion:
	- only include `if :: ... -> Bool` if there are `Bool` primitives
	- include `False` and `True` automatically only on Speculate's background
* add code-optional candidate nubbing and debug functions


v0.2.2
------

* by default, search for 60 argument combinations
  among 100000 enumerated combinations


v0.2.0
------

* search until 100% match is found and exit
* other misc changes


v0.1.2
------

For the changelog of earlier versions, check the git commit history.
