TODO for Conjure
================

A non-exhaustive list of things TO DO for Conjure.

* report number of "hit" assignments, add ill example as a bench

* refactor `candidateExprs` and `conjpureWith`.
  Move down conjuring of `(===)` from `conjpureWith` into `candidateExprs`.

* refactor `conjureTiersFor` to use `conjureMaybeTiersFor`

* implement `conjureHasTiers` and use it on `conjureMkEquation`;
  this will make it easy to backport unique candidateExprs from the erased
  commit back into the tool: just `discardLaterT (===)`.
  Will this impact performance a bit?  I don't think so.


### for later

* allow conjuring from tests instead of partial definitions?

* allow conjuring from partially defined implementations?

    partial :: ((Int -> Int) -> Int -> Int) -> (Int -> Int)
    partial impl  =  f
      where
      f n  =  if n == 0
              then 1
              else impl f n


This file is part of Conjure,
(C) 2021 Rudy Matela,
Distribued under the 3-Clause BSD license.
