TODO for Conjure
================

A non-exhaustive list of things TO DO for Conjure.

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


## Prune root-recursion

Two of the following are redundant:

	xs ?? ys  =  []

	xs ?? []  =  []
	xs ?? (x:ys)  =  xs ?? ys

	xs ?? []  =  []
	xs ?? (x:ys)  =  ys ?? xs

So is the second of the following:

	xs ?? ys  =  ys

    [] ?? xs  =  xs
    (x:xs) ?? ys  =  xs ?? ys

In the examples above,
recursive calls on the root eventually
lead to a final fixed base-case value.


This file is part of Conjure,
(C) 2021-2024 Rudy Matela,
Distribued under the 3-Clause BSD license.
