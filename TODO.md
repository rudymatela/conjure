TODO for Conjure
================

A non-exhaustive list of things TO DO for Conjure.

* Allow timeout setting?

* Find most efficient of a given size (see below).

* forbid recursion into negatives (see below)

* fix inefficiency in `enumerateAppsFor` in the presence of guards (see below).

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

Alternative: include the guard as part of the pattern internally.  As if we
had a n+k pattern.  Like so:

	tri 1  =  1
	tri x@(_ + 2)  =  x + tri (x - 1)

When displaying of course, we remove the n+k pattern.

This can be done as early as enumerating the possible patterns and should make
things easier in `keepB`.  The matching engine would have to be customized
somehow.  Maybe an `@`-named operator can adopt a special meaning in `Express`?


## Fix inefficiency of `enumerateAppsFor` with guards

When guards are present, `enumerateAppsFor` sometimes produces an empty trail of tiers.
You can reproduce the bug with:

1. comment-out `. insemptier (conjureUndefined f)` in `Conjure.Engine`;
2. run `leftmost` from `eg/tree.hs`;
3. you get an empty trail of tiers whenever the guard is present.

To fix this `enumerateAppsFor` has to be re-implemented without using `filterT`:
perhaps with an actual implementation for `ufor`.

Conjecture: this won't improve runtime significantly, we only see the impact of
the empty trail of tiers later in the enumeration.  I'll fix when I have the
time.



This file is part of Conjure,
(C) 2021-2025 Rudy Matela,
Distribued under the 3-Clause BSD license.
