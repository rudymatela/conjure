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

* Allow chains of guards (see below).

* Prefer smaller recursive calls (see below).

* Find most efficient of a given size (see below).

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


## Find the most efficient of a given size

While evaluating candidates, we can count the number of matches in order to
select the most efficient.  When actually conjuring, we always wait until the
end of the current size to select the candidate with least evaluations.


## Prefer candidates with smaller recursive calls

For now, this is the solution found for `merge`:

	merge [] xs  =  xs
	merge (x:xs) []  =  x:xs
	merge (x:xs) (y:ys)
	  | x <= y  =  x:merge xs (y:ys)
	  | otherwise  =  merge (y:x:xs) ys

This is still of the `O(n)` complexity class, but there's a needless comparison
in the otherwise clause.  When of the same overall size, Conjure should prefer
candidates with smaller recursive calls like so:

	| otherwise  =  y:merge (x:xs) ys

In the enumeration itself, we should add a function `catconMapT` that would
make the recursion fillings of `otherwise = _` appear later than the recursion
fillings of `otherwise = y:_` when of the same size.  We perhaps need something
like:

	partialCands ++ forN partialCands

The above is so we don't "delay" fully-defined non-recursive candidates.

While I am at this, maybe partial candidates should be included in the final
list of candidates, with its tests being skipped.


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
