TODO for Conjure
================

A non-exhaustive list of things TO DO for Conjure.

* `merge` and `msort`

* implement memoization and loop detection on deval?

  0. keep a memo list `:: (Expr, Maybe Dynamic)`;
  1. lookup in in the memo;
  2. if the `Expr` is bound to `Just` a `Dynamic`, return it;
  3. if the `Expr` is bound to `Nothing`, exit with `infinite loop`
  4. if there is no binding carry on
  5. if the max number of memoes has been reached, exit with `max recursions`
  6. create a binding `(Expr,Nothing)`
  7. recursively evaluate `Expr`
  8. replace the binding by `(Expr,Dynamic)`
  9. return the computed `Dynamic` value

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
