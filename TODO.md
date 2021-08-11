TODO for Conjure
================

A non-exhaustive list of things TO DO for Conjure.

* see `access` example on terpret, prettyprint the type signature correctly
  as `access :: ...` instead of prefixing the infix name
  (EDIT: this bug is on express itself.  Type signatures for infix variables
  are printed incorrectly)

* remove `requireDescent=False` requirement from `gcd`
  (add and use `isDeconstruction`)
  this would also eliminate the requirement of providing `dec`

* consider not breaking in some cases (increased crossproduct of patterns)


### for later

* allow specifying properties that need to be true

* allow recursion under any lazy functions (discover them by testing!)

* pretty-print top-level ifs?

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
