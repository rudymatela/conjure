TODO for Conjure
================

A non-exhaustive list of things TO DO for Conjure.

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
