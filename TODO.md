TODO for Conjure
================

A non-exhaustive list of things TO DO for Conjure.

* cleanup repo for GitHub release

* fill in the blanks on the README

For release
-----------

* create standalone repo

* release on GitHub

* add travis file

* add some tutorial Haddock, replicate on README

* release on Hackage


For later
---------

* reduce to unique candidateExprs through testing!  we should only enumerate what is needed

* use FitSpec's notation of `"factorial n"` to name the variables
  instead of just `"factorial"`

* Automatically include ifs?

	- problem, typeclass folding is not possible for result values.

* some food for thought:

    -- think about using a partially implemented function to make it easy to find
    food_for_thought :: ((Int -> Int) -> Int -> Int) -> (Int -> Int)
    food_for_thought impl  =  f
      where
      f n  =  if n == 0
              then 1
              else impl f n


This file is part of Conjure,
Copyright 2021 Rudy Matela,
Distribued under the 3-Clause BSD license.
