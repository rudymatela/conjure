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

Given

	square :: Int -> Int
	square 0  =  0
	square 1  =  1
	square 2  =  4

and

	primitives :: [Expr]
	primitives  =  [ val (0::Int)
	               , val (1::Int)
	               , value "+" ((+) :: Int -> Int -> Int)
	               , value "*" ((*) :: Int -> Int -> Int)
	               ]

running

	> conjure "square" square primitives

yields

	square :: Int -> Int
	-- testing 3 combinations of argument values
	-- looking through 3 candidates of size 1
	-- looking through 3 candidates of size 2
	-- looking through 5 candidates of size 3
	square x  =  x * x

in less than a second.

See the `eg/arith.hs` example.


Conjuring recursive functions
-----------------------------

Given

	factorial :: Int -> Int
	factorial 0  =  1
	factorial 1  =  1
	factorial 2  =  2
	factorial 3  =  6
	factorial 4  =  24
	factorial 5  =  120

and

	primitives :: [Expr]
	primitives  =  [ val (0::Int)
	               , val (1::Int)
	               , value "+" ((+) :: Int -> Int -> Int)
	               , value "*" ((*) :: Int -> Int -> Int)
	               , value "dec" (subtract 1 :: Int -> Int)
	               , value "==" ((==) :: Int -> Int -> Bool)
	               ]

running

	> conjure "factorial" factorial primitives

yields

	factorial :: Int -> Int
	-- testing 6 combinations of argument values
	-- looking through 3 candidates of size 1
	-- looking through 5 candidates of size 2
	-- looking through 8 candidates of size 3
	-- looking through 26 candidates of size 4
	-- looking through 59 candidates of size 5
	-- looking through 167 candidates of size 6
	-- looking through 581 candidates of size 7
	-- looking through 1654 candidates of size 8
	-- looking through 5754 candidates of size 9
	-- looking through 17797 candidates of size 10
	factorial n  =  if n == 0 then 1 else n * factorial (dec n)

in about 3 seconds.

See the `eg/factorial.hs` example.

It is also possible to generate:

    factorial x  =  if x == 0 then 1 else x * factorial x - 1

in about 30s by changing the primitives and increasing the size limit.


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
