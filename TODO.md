TODO for Conjure
================

A non-exhaustive list of things TO DO for Conjure.

* return a `Results` record from conjpure0With

* add `Args` switch to show allowed deconstructions

* fix infinite recursions through unmatches zeroes (see below)

* improve documentation of `conjureIsDeconstruction`.
  Document behaviour outside of function, not inside of it.

* consider non top-level cases


## Infinite recursions through unmatched zeroes

Consider the following:

	foo 1  =  1
	foo n  =  n + foo (n - 1)

In this case, we want the function to be undefined for 0 with a guard.  Perhaps
implicitly in the inner workings, perhaps explicitly through a custom show
instance too...  I have to think about this.


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
