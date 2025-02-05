TODO for Conjure
================

A non-exhaustive list of things TO DO for Conjure.

* bump copyright year

* review main haddock documentation

* consider non top-level cases


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
