TODO for Conjure
================

A non-exhaustive list of things TO DO for Conjure.

* generate functions with top-level case patterns:

        len []  =  0
        len (x:xs)  =  1 + len xs

	1. add `conjureExpr` to `Conjurable`


### for later

* add machinery to reify `Int -> Int` from the `(Expr,Expr)` definition

* allow specifying properties that need to be true

* allow recursion under any lazy functions (discover them by testing!)

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
