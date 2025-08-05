TODO for Conjure
================

A non-exhaustive list of things TO DO for Conjure.

* Make so that derived Listable instances `reset`
  constructors that are not recursive.
  This change will need to be done in LeanCheck itself.

* Detail two new proposed pruning rules from Colin.
  (email from Feb 24 and follow-up in meeting)

* Allow timeout setting?

* Rethink Conjurable typeclass?

* Find most efficient of a given size (see below).

* Better error reporting when `Listable` is out-of-scope when using `deriveConjurable`.
  This needs to be implemented on LeanCheck itself.

* forbid recursion into negatives (see below)

* Add way to consider functions that don't increase size of arguments in recursive calls
	(qsort example)


## Find the most efficient of a given size

While evaluating candidates, we can count the number of matches in order to
select the most efficient.  When actually conjuring, we always wait until the
end of the current size to select the candidate with least evaluations.


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

Alternative: include the guard as part of the pattern "internally".  As if we
had a n+k pattern.  Like so:

	tri 1  =  1
	tri x@(_ + 2)  =  x + tri (x - 1)

When displaying of course, we remove the n+k pattern.

This can be done as early as enumerating the possible patterns and should make
things easier in `keepB`.  The matching engine would have to be customized
somehow.  Maybe an `@`-named operator can adopt a special meaning in `Express`?



This file is part of Conjure,
(C) 2021-2025 Rudy Matela,
Distribued under the 3-Clause BSD license.
