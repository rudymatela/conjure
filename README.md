Conjure
=======

[![Conjure's Build Status][build-status]][build-log]
[![Conjure on Hackage][hackage-version]][code-conjure-on-hackage]
[![Conjure on Stackage LTS][stackage-lts-badge]][code-conjure-on-stackage-lts]
[![Conjure on Stackage Nightly][stackage-nightly-badge]][code-conjure-on-stackage-nightly]

![Conjure logo][conjure-logo]

Conjure is a tool that produces Haskell functions out of partial definitions.

This is currently an experimental tool in its early stages,
don't expect much from its current version.
It is just a piece of curiosity in its current state.


Installing
----------

To install the [latest Conjure version from Hackage], just run:

	$ cabal update
	$ cabal v1-install code-conjure

If you are using Cabal v3.0 or later,
[avoid using `cabal install`] for the time being
and use `v1-install` instead.

Prerequisites are [express], [leancheck] and [speculate].
They should be automatically resolved and installed by [Cabal].

NOTE: the name of the Hackage package is __[`code-conjure`]__
-- not to be confused with [Conjure the BitTorrent client].


Conjuring functions
-------------------

You first need to import the library with:

	import Conjure

Then, given

	square :: Int -> Int
	square 0  =  0
	square 1  =  1
	square 2  =  4

and

	primitives :: [Prim]
	primitives  =  [ pr (0::Int)
	               , pr (1::Int)
	               , prim "+" ((+) :: Int -> Int -> Int)
	               , prim "*" ((*) :: Int -> Int -> Int)
	               ]

running

	> conjure "square" square primitives

yields

	square :: Int -> Int
	-- testing 3 combinations of argument values
	-- pruning with 14/25 rules
	-- looking through 3 candidates of size 1
	-- looking through 4 candidates of size 2
	-- looking through 9 candidates of size 3
	square x  =  x * x

in less than a second.

See the `eg/arith.hs` example.


Conjuring recursive functions
-----------------------------

Given

	factorial :: Int -> Int
	factorial 1  =  1
	factorial 2  =  2
	factorial 3  =  6
	factorial 4  =  24

and

	primitives :: [Prim]
	primitives  =  [ pr (0::Int)
	               , pr (1::Int)
	               , prim "+" ((+) :: Int -> Int -> Int)
	               , prim "*" ((*) :: Int -> Int -> Int)
	               , prim "-" ((-) :: Int -> Int -> Int)
	               ]

running

	> conjure "factorial" factorial primitives

yields

	factorial :: Int -> Int
	-- testing 4 combinations of argument values
	-- pruning with 27/65 rules
	-- looking through 3 candidates of size 1
	-- looking through 4 candidates of size 2
	-- looking through 13 candidates of size 3
	-- looking through 34 candidates of size 4
	-- looking through 75 candidates of size 5
	-- looking through 183 candidates of size 6
	-- looking through 577 candidates of size 7
	factorial 0  =  1
	factorial x  =  x * factorial (x - 1)

in less than a second.

It is also possible to generate

	factorial x  =  foldr (*) 1 [1..x]

by including `enumFromTo` and `foldr` in the background.

See the `eg/factorial.hs` example.


Conjuring from specifications (duplicates)
------------------------------------------

In some cases,
a partial definition may not be appropriate
for one of two reasons:

1. Conjure may fail to "hit" the appropriate data points;
2. specifying argument-result bindings may not be easy.

Take for example a function `duplicates :: Eq a => [a] -> [a]`
that should return the duplicate elements in a list without repetitions.

Let's start with the primitives:

	primitives :: [Prim]
	primitives  =  [ pr ([] :: [Int])
	               , prim "not" not
	               , prim "&&" (&&)
	               , prim ":" ((:) :: Int -> [Int] -> [Int])
	               , prim "elem" (elem :: Int -> [Int] -> Bool)
	               , prif (undefined :: [Int])
	               ]

Now here's a first attempt at a partial definition:

	duplicates' :: [Int] -> [Int]
	duplicates' []  =  []
	duplicates' [1,2,3,4,5]  =  []
	duplicates' [1,2,2,3,4]  =  [2]
	duplicates' [1,2,3,3,3]  =  [3]
	duplicates' [1,2,2,3,3]  =  [2,3]

Here's what `conjure` prints:

	> conjureWith args{maxSize=18} "duplicates" duplicates primitives
	duplicates :: [Int] -> [Int]
	-- testing 1 combinations of argument values
	-- pruning with 21/26 rules
	-- looking through 2 candidates of size 1
	duplicates xs  =  xs

The generated function clearly does not follow our specification.
But if we look at the number of tests,
we see that only _one_ of the argument-result bindings
of our partial definition was used.
Conjure failed to hit any of the argument values lists of 5 elements.
(since Conjure uses enumeration to test function
these values have to be kept "small").

Let's retry:

	duplicates :: [Int] -> [Int]
	duplicates [0,0]  =  [0]
	duplicates [0,1]  =  []
	duplicates [1,0,1]  =  [1]

Here's what `conjure` now prints:

	> conjureWith args{maxSize=18} "duplicates" duplicates primitives
	duplicates :: [Int] -> [Int]
	-- testing 3 combinations of argument values
	-- pruning with 21/26 rules
	-- ...
	-- looking through 16 candidates of size 9
	duplicates []  =  []
	duplicates (x:xs)  =  if elem x xs then [x] else []

The `duplicates` function that Conjure generated is still not correct.
Nevertheless, it does follow our partial definition.  We have to refine it:

	duplicates :: [Int] -> [Int]
	duplicates [0,0]  =  [0]
	duplicates [0,1]  =  []
	duplicates [1,0,1]  =  [1]
	duplicates [0,1,0,1]  =  [0,1]

Here's what Conjure prints:

	duplicates []  =  []
	duplicates (x:xs)  =  if elem x xs then x:duplicates xs else []

This implementation follows our definition, but may return duplicate duplicates,
see:

	duplicates [1,0,1,0,1]  =  [1,0,1]

Let's do one final refinement:

	duplicates :: [Int] -> [Int]
	duplicates [0,0]  =  [0]
	duplicates [0,1]  =  []
	duplicates [1,0,1]  =  [1]
	duplicates [0,1,0,1]  =  [0,1]
	duplicates [1,0,1,0,1]  =  [0,1]
	duplicates [0,1,2,1]  =  [1]

Now Conjure prints a correct implementation:

	> conjureWith args{maxSize=18} "duplicates" duplicates primitives
	duplicates :: [Int] -> [Int]
	-- testing 6 combinations of argument values
	-- ...
	-- looking through 2189 candidates of size 17
	duplicates []  =  []
	duplicates (x:xs)  =  if elem x xs && not (elem x (duplicates xs)) then x:duplicates xs else duplicates xs
	(in 1.5s)

The above is a correct implementation.

In this case,
specifying the function with specific argument-result bindings
is perhaps not the best approach.

Specifying test properties perhaps better describes what we want.
Again, we would like `duplicates` to return all duplicate elements
without repetitions.  Let's encode this in a function using [LeanCheck]:

	import Test.LeanCheck (holds)

	duplicatesSpec :: ([Int] -> [Int]) -> Bool
	duplicatesSpec duplicates  =  and
	  [ holds 360 $ \x xs -> (count (x ==) xs > 1) == elem x (duplicates xs)
	  , holds 360 $ \x xs -> count (x ==) (duplicates xs) <= 1
	  ]  where  count p  =  length . filter p

This function takes as argument a candidate implementation of `duplicates`
and returns whether it is valid.

Then we can use the function `conjureFromSpecWith` to generate the same duplicates function
passing our `duplicatesSpec`:

	> conjureFromSpecWith args{maxSize=18} "duplicates" duplicatesSpec primitives
	duplicates :: [Int] -> [Int]
	duplicates []  =  []
	duplicates (x:xs)  =  if elem x xs && not (elem x (duplicates xs)) then x:duplicates xs else duplicates xs
	(in 1.5s)


Related work
------------

[MagicHaskeller] (2007) is another tool
that is able to generate Haskell code automatically.
It supports recursion through
catamorphisms, paramorphisms and the [fix] function.
It is more mature than Conjure and is several orders of magnitude faster.

[Barliman] for Lisp is another tool that does program synthesis.

There are hundreds of others,
I'll add the most closely related here when I have the time.


Further reading
---------------

For a detailed documentation of each function, see
[Conjure's Haddock documentation].


Conjure, Copyright 2021  Rudy Matela,
distribued under the 3-clause BSD license.


[Conjure's Haddock documentation]: https://hackage.haskell.org/package/code-conjure/docs/Conjure.html
[fix]: https://hackage.haskell.org/package/base/docs/Data-Function.html#v:fix

[symbol `>`]: https://www.haskell.org/haddock/doc/html/ch03s08.html#idm140354810780208
[Template Haskell]: https://wiki.haskell.org/Template_Haskell

[conjure-logo]: https://github.com/rudymatela/conjure/raw/master/doc/conjure.svg?sanitize=true

[`code-conjure`]:                   https://hackage.haskell.org/package/code-conjure
[Conjure the BitTorrent client]:    https://hackage.haskell.org/package/conjure

[Cabal]:   https://www.haskell.org/cabal
[Haskell]: https://www.haskell.org/
[leancheck]:      https://hackage.haskell.org/package/leancheck
[LeanCheck]:      https://hackage.haskell.org/package/leancheck
[express]:        https://hackage.haskell.org/package/express
[speculate]:      https://hackage.haskell.org/package/speculate
[MagicHaskeller]: https://hackage.haskell.org/package/MagicHaskeller
[Barliman]:       https://github.com/webyrd/Barliman

[avoid using `cabal install`]:         https://github.com/haskell/cabal/issues/7373
[latest Conjure version from Hackage]: https://hackage.haskell.org/package/code-conjure

[build-log]:    https://github.com/rudymatela/conjure/actions/workflows/build.yml
[build-status]: https://github.com/rudymatela/conjure/actions/workflows/build.yml/badge.svg
[hackage-version]:                  https://img.shields.io/hackage/v/code-conjure.svg
[code-conjure-on-hackage]:          https://hackage.haskell.org/package/code-conjure
[stackage-lts-badge]:               https://stackage.org/package/code-conjure/badge/lts
[stackage-nightly-badge]:           https://stackage.org/package/code-conjure/badge/nightly
[code-conjure-on-stackage]:         https://stackage.org/package/code-conjure
[code-conjure-on-stackage-lts]:     https://stackage.org/lts/package/code-conjure
[code-conjure-on-stackage-nightly]: https://stackage.org/nightly/package/code-conjure
