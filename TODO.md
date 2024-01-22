TODO for Conjure
================

A non-exhaustive list of things TO DO for Conjure.

* Review `bench/redundants.out` and prune redundant candidates.
  See sections below for ideas.


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
