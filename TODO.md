TODO for Conjure
================

A non-exhaustive list of things TO DO for Conjure.

* reduce to unique `candidateExprs` through testing.
  We should only enumerate functions that were tested to be different.
  We can limit the tests to the ones that are defined in the given function.

* use FitSpec's notation of `"factorial n"` to name the variables
  instead of just `"factorial"`

* use partially defined implementations?

    partial :: ((Int -> Int) -> Int -> Int) -> (Int -> Int)
    partial impl  =  f
      where
      f n  =  if n == 0
              then 1
              else impl f n


This file is part of Conjure,
(C) 2021 Rudy Matela,
Distribued under the 3-Clause BSD license.
