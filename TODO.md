TODO for Conjure
================

A non-exhaustive list of things TO DO for Conjure.

* discard erroneous candidates from `bench/erroneous.txt` somehow

* consider non top-level cases

* consider pruning earlier with `productsLaterThat`
  replacing the call to `products` in `Conjure.Engine`

* Review `bench/redundants.txt` and prune redundant candidates.
  See sections below for ideas.


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
