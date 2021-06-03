TODO for Conjure
================

A non-exhaustive list of things TO DO for Conjure.


* allow recursion under `&&` and `||` in case of boolean-valued functions;

* top-down generation of candidate expressions

* generate functions with top-level case patterns:

        len []  =  0
        len (x:xs)  =  1 + len xs

* only allow recursion on deconstructed arguments:

        foo (x:xs)  =  ... len xs ...

  this will require generation of functions with top-level case patterns.


### for later

* allow recursion under any lazy functions (discover them by testing!)

* exclude magic numbers?  e.g.: `1+1`?

* allow conjuring from partially defined implementations?

        partial :: ((Int -> Int) -> Int -> Int) -> (Int -> Int)
        partial impl  =  f
          where
          f n  =  if n == 0
                  then 1
                  else impl f n

* consider leveraging lazyness somehow?
  (related to allowing recursion under any lazy functions)

* consider leveraging polymorphism somehow?


This file is part of Conjure,
(C) 2021 Rudy Matela,
Distribued under the 3-Clause BSD license.
