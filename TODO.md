TODO for Conjure
================

A non-exhaustive list of things TO DO for Conjure.

* improve documentation of `conjureIsDeconstruction`.
  Document behaviour outside of function, not inside of it.

* Discard erroneous candidates from `bench/erroneous.txt` somehow.
  See sections below for ideas.

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


This file is part of Conjure,
(C) 2021-2024 Rudy Matela,
Distribued under the 3-Clause BSD license.
