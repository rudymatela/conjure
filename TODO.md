TODO for Conjure
================

A non-exhaustive list of things TO DO for Conjure.

* remove magic number (take 12) in keepBndn/keepBase:
  replace `earlyTests` setting with `maxEarlyTests`;
  setting `0` would mean no early tests.

* Make so that derived Listable instances `reset`
  constructors that are not recursive.
  This change will need to be done in LeanCheck itself.

* Detail two new proposed pruning rules from Colin.
  (email from Feb 24 and follow-up in meeting)

* Allow timeout setting?

* Rethink Conjurable typeclass?

* Allow chains of guards (see below).

* Better error reporting when `Listable` is out-of-scope when using `deriveConjurable`.
  This needs to be implemented on LeanCheck itself.

* forbid recursion into negatives (see below)

* Add way to consider functions that don't increase size of arguments in recursive calls
	(qsort example)


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
