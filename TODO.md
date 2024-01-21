TODO for Conjure
================

A non-exhaustive list of things TO DO for Conjure.

* In `Conjure.Defn`,
  add and use `isUndefinedBndn`, `isUndefinedDefn`
  which check whether a `Bndn` or a `Defn` has
  undefined variables (those not present in patterns).
  These functions will be particularly useful
  in `isRedundantByRepetition` and `isRedundantByDefn`.
  In the case of `isRedundantByRepetition` it may even
  "unprune" some candidates that should not have
  been pruned in the first place.

* Sweep some "easy" TODO markings scattered throughout the code

* properly document why isRedundantByIntroduction
  is does not pay-off to use and remove idea section here.
  While doing this, commit-and-revert to record the effect
  on `*.out` files and runtimes.

* Review `bench/redundants.out` and prune redundant candidates.
  See sections below for ideas.


## Prune modulo variable introduction

The following is redundant:

	goo []  =  []
	goo (x:xs)  =  x:xs

It is equivalent to:

	goo xs  =  xs

The following is also redundant:

	[] ?? xs  =  []
	(x:xs) ?? ys  =  x:xs

It is equivalent to:

	xs ?? ys  =  xs

One possible path would be to,
for each pattern argument,
we check if variable introduction is possible.
If it is, we check that variable introduction
is also possible when all other pattern arguments are the same
and the results would be the same.
This is straightforward, but tricky to implement.

See `isRedundantByIntroduction`.


## Prune modulo rewriting

The following is redundant, as only the second equation is necessary:

	foo 0  =  0
	foo x  =  x + x

Another example is:

	foo 0  =  1
	foo x  =  x + 1

The tricky part is when dealing with multiple arguments:

	x ? 0  =  x
	x ? y  =  x + y

One possible path would be to replace 0 in the second equation
then use the Theory to discover x + 0 is equal to 0.


## Prune modulo symbolic execution

The following can be eliminated by simple application of the recursive call:

	xs ?? []  =  []
	xs ?? (x:ys)  =  xs ?? []

... but then we are nearing the realm of testing candidates themselves
in order to prune to avoid testing!


## Prune magic

Consider excluding non-atomic magic numbers?  e.g.: `1+1`?


This file is part of Conjure,
(C) 2021-2024 Rudy Matela,
Distribued under the 3-Clause BSD license.
