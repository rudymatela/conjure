TODO for Conjure
================

A non-exhaustive list of things TO DO for Conjure.

* Better error reporting when `Listable` is out-of-scope when using `deriveConjurable`.
  This needs to be implemented on LeanCheck itself.

* forbid recursion into negatives

* consider the size of patterns to thin-out each size partition

* consider non top-level cases


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


## Thin-out size partitions

Consider the following two candidates:

	fib01 x y z  =  dec x

	fib01 x y 0  =  x
	fib01 x y z  =  y

They both currently have size 2.
Perhaps the second should be bigger than the first?
"Simple" solution:
consider the number of (extra) patterns as part of the size.

See the following two candidates of size 3:

	fib01 x y z  =  x + x

	fib01 x y 0  =  x
	fib01 x y z  =  dec x

The same thing can be said.

Now a little bit more complex...
The following two candidates appear at size 3 for fib01:

	fib01 x 0 y  =  x
	fib01 x y 0  =  y
	fib01 x y z  =  0

	fib01 x 0 0  =  x
	fib01 x 0 y  =  y
	fib01 x y z  =  0

Shouldn't these two have different size?
The second feels bigger than the first.
Maybe the size of non-variable patterns should be taken into account.

This relates to the eg/fib01 example.

Also:
see the allowed patterns on the Int functions of two arguments
in bench/candidates.txt.  Search for "allowed patterns".

There are two levels here:

1. considering the number of patterns as part of the size
2. considering the number of non-variable LHS arguments as part of the size

Level 2. is not addressed at all.  But it turns out that level 1. is already
handled at the conjurePatterns function.  The patterns reported with showPatterns
and on bench/candidates already seem to be divided considering the number of
lines...

Something to keep in mind: tiers enumerations have size "0".
If one wants to increase size, one needs to push things to size "1".
So the size is actually being considered.  One would need a delayedProducts to
complete "1.".  Maybe it is better to start investigating from "2."

The enumeration currently works like so:
the size of patterns is defined by the number of patterns that are allowed to
have _another_ option other than a simple variable.
Perhaps these should be thinned-out in post processing to achieve "2."?


## Non top-level cases

Consider allowing non top-level cases,
so that functions like the following are reachable:

	last []  =  undefined
	last [x]  =  x
	last (x:y:xs)  =  last (y:xs)

Perhaps the negative runtime impact would not be so great
given the delayed tiers enumeration,
and we would avoid some `prif` hacks
needed to conjure some functions.

The one downside is that this would take a few days of work to implement.


This file is part of Conjure,
(C) 2021-2025 Rudy Matela,
Distribued under the 3-Clause BSD license.
