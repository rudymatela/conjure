TODO for Conjure
================

A non-exhaustive list of things TO DO for Conjure.

* filter out redundant cases like the following:

	union Leaf Leaf  =  Leaf
	union Leaf (Node t1 x t2)  =  Leaf
	union (Node t1 x t2) Leaf  =  t1
	union (Node t1 x t2) (Node t3 y t4)  =  t1

  the above is equivalent to simply:

	union Leaf           _  =  Leaf
	union (Node t1 x t2) _  =  t1

  which should be already enumerated anyway.

* test `conjureCases` from `deriveConjurable`;

* consider not breaking in some cases (increased crossproduct of patterns)

* reduce the number of `deconstructions` considered:

	1. place a `traceShowId` in `deconstructions :: [Expr]`
	2. run the GCD example

		[_ `mod` y :: Int,x `mod` _ :: Int,_ `mod` x :: Int,y `mod` _ :: Int,0 `mod` _ :: Int,0 `mod` _ :: Int]

	3. there's no need to go variable by variable.  Just generating expressions
	   with a _single hole_ should be enough.



### for later

* use feedback from testing to prune candidates,
	there are a few ways of doing this:

	1. inspect what happens after the fact and restart enumeration with some
		pattern results fixed.

	2. replace the Spec interface by something that returns a list of booleans
		instead of a single boolean:

			conjureFromSpec :: Conjurable f => String -> (f -> [Bool]) -> [Prim] -> IO ()

		It is hard to reason about the result of tests if the spec is just `f
		-> Bool`.  Having it granulated with several booleans is beneficial,
		because for each boolean we can compute its three results:

			False
			True
			error/bottom/undefined

		From a specific pattern result (ps2fss/p2eess, near copyBindings) we
		can build a partial definition with just the value for that single
		pattern defined:

			factorial 1  =  3
			factorial x  =  undefined

		If we test this function using the Spec and we get a single proper
		`False` result, this means that the pattern cannot appear in the
		desired definition.  If we get no False, but only True or undefined
		values there's nothing we can say about the pattern and it is kept in
		the enumeration.

		This should reduce the search space specially on the `eg/bst, union`
		example.  For reference, if I manually prune the search space by
		limiting ourselves with functions with less than 2 patterns the desired
		union definition is found.

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
