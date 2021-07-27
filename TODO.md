TODO for Conjure
================

A non-exhaustive list of things TO DO for Conjure.

* fix `conjureEvaluate :: ... a->b ...` to make it support more than one argument

* consider memoizing `recs ep` in `candidateDefnsC`
  and a sub function with `vs` arguments.

* remove `requireDescent=False` requirement from `gcd`
  (add and use `isDeconstruction`)
  this would also eliminate the requirement of providing `dec`

* consider not breaking in some cases (increased crossproduct of patterns)


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
