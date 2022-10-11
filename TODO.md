TODO for Conjure
================

A non-exhaustive list of things TO DO for Conjure.

* reduce the number of `deconstructions` considered:

	1. place a `traceShowId` in `deconstructions :: [Expr]`
	2. run the GCD example

		[_ `mod` y :: Int,x `mod` _ :: Int,_ `mod` x :: Int,y `mod` _ :: Int,0 `mod` _ :: Int,0 `mod` _ :: Int]

	3. there's no need to go variable by variable.  Just generating expressions
	   with a _single hole_ should be enough.



### for later

* pretty-print top-level if and case expressions?

* consider not breaking arguments in some cases
  (increased crossproduct of patterns).
  but which cases?

* exclude magic numbers?  e.g.: `1+1`?

* allow conjuring from partially defined implementations?

        partial :: ((Int -> Int) -> Int -> Int) -> (Int -> Int)
        partial impl  =  f
          where
          f n  =  if n == 0
                  then 1
                  else impl f n

* consider discover orderings on arguments?

* consider leveraging lazyness somehow?
  (related to allowing recursion under any lazy functions)

* consider leveraging polymorphism somehow?

* consider allowing lambdas that introduce free variables?


This file is part of Conjure,
(C) 2021 Rudy Matela,
Distribued under the 3-Clause BSD license.
