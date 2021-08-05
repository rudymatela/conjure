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
	               , prim "dec" (subtract 1 :: Int -> Int)
	               ]

running

	> conjure "factorial" factorial primitives

yields

	factorial :: Int -> Int
	-- testing 4 combinations of argument values
	-- pruning with 22/42 rules
	-- looking through 3 candidates of size 1
	-- looking through 6 candidates of size 2
	-- looking through 16 candidates of size 3
	-- looking through 39 candidates of size 4
	-- looking through 78 candidates of size 5
	-- looking through 166 candidates of size 6
	factorial 0  =  1
	factorial x  =  x * factorial (dec x)

in less than a second.

It is also possible to generate

	factorial x  =  foldr (*) 1 [1..x]

by including `enumFromTo` and `foldr` in the background.

See the `eg/factorial.hs` example.


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
