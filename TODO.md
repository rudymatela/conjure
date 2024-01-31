TODO for Conjure
================

A non-exhaustive list of things TO DO for Conjure.

* Discard erroneous candidates from `bench/erroneous.txt` somehow.
  See sections below for ideas.

* consider non top-level cases


## Erroneous recursions

The `descends` function still lets some invalid functions pass through:

	[] ?? xs  =  xs
	(x:xs) ?? []  =  xs
	(x:xs) ?? (y:ys)  =  ys ?? (x:ys)
	-- [0] ?? [0,0]  =  bottom

	[] ?? xs  =  xs
	(x:xs) ?? []  =  xs
	(x:xs) ?? (y:ys)  =  ys ?? (y:ys)
	-- [0] ?? [0,0]  =  bottom

In both of these functions, nothing is being deconstructed.
We have to catch and prune these.


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
