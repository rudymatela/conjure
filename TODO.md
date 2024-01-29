TODO for Conjure
================

A non-exhaustive list of things TO DO for Conjure.

* Review `bench/redundants.txt` and prune redundant candidates.
  See sections below for ideas.


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
