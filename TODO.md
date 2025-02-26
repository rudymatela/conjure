TODO for Conjure
================

A non-exhaustive list of things TO DO for Conjure.

soon
----

* Add regression test for empty enumeration bug

* Quick fix for empty enumeration bug

* Proper fix for empty enumeration bug

* Check derivation of either enumeration...

* Warn when there are no tests!
  Test high order functions.


later
-----

* Add `test/conjure.hs` with some basic conjuring...

* Fix bug with pairwise in eg/tuple

* On `test/derive.hs`, fix infinite loop on the RN instance

* Allow timeout setting?

* Rethink Conjurable typeclass?

* Rename primitives to ingredients?

* Allow chains of guards (see below).

* Move `Args` into `[Prim]`?

* Better error reporting when `Listable` is out-of-scope when using `deriveConjurable`.
  This needs to be implemented on LeanCheck itself.

* forbid recursion into negatives (see below)

* Warn when no tests are present somehow?


## Allow chains of guards

With the use of `guard` right now,
Conjure can generate functions such as the following:

	function x y z
	  | x < 123    =  ...
	  | otherwise  =  ...

We should probably allow chains of guards such as the following:

	function x y z
	  | x < 123    =  ...
	  | x == 321   =  ...
	  | y == 12    =  ...
	  | otherwise  =  ...

Internally, these are just chains of if-then-else applications:

	function x y z  =  if x < 123
	                   then ...
					   else if x == 321
					   then ...
					   else if y == 12
					   then ...
					   else ...

This change shouldn't be so complicated to introduce
requiring a change in `enumerateAppsFor` relaxing `ufor hx`
depending on what we have on the left...


## Forbid recursion into negatives

Instead of reporting:

	tri 1  =  1
	tri x  =  x + tri (x - 1)

Report:

	tri 1          =  1
	tri x | x > 1  =  x + tri (x - 1)

This is not trivial to implement.
Tentative steps:

1. create a `descents` function, similar to `descends`,
   that is able to list the groups of variable that have recursive descents.
   This will somehow need a dummy `isDecOf` function.
   This may require changing the format definitions themselves...
2. use this on `showDefn` somehow
3. use this on `toDynamicWithDefn` somehow


This file is part of Conjure,
(C) 2021-2025 Rudy Matela,
Distribued under the 3-Clause BSD license.
