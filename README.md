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
	$ cabal install code-conjure

Starting from Cabal v3.0, you need to pass `--lib` as an argument to cabal
install:

	$ cabal install code-conjure --lib

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

	background :: [Expr]
	background =
	  [ val (0::Int)
	  , val (1::Int)
	  , value "+" ((+) :: Int -> Int -> Int)
	  , value "*" ((*) :: Int -> Int -> Int)
	  , value "==" ((==) :: Int -> Int -> Bool)
	  ]

running

	> conjure "square" square background

yields

	square :: Int -> Int
	-- looking through 815 candidates, 100% match, 3/3 assignments
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

	background :: [Expr]
	background  =
	  [ val (0::Int)
	  , val (1::Int)
	  , value "+" ((+) :: Int -> Int -> Int)
	  , value "*" ((*) :: Int -> Int -> Int)
	  , value "dec" (subtract 1 :: Int -> Int)
	  , value "isZero" ((==0) :: Int -> Bool)
	  , val False
	  , val True
	  , ifFor (undefined :: Int)
	  , value "==" ((==) :: Int -> Int -> Bool)
	  ]

running

	> conjure "factorial" factorial background

yields

	factorial :: Int -> Int
	-- looking through 9266 candidates, 100% match, 6/6 assignments
	factorial x  =  if isZero x then 1 else x * factorial (dec x)

in about 3 seconds.

See the `eg/factorial.hs` example.

It is also possible to generate:

    factorial x  =  if x == 0 then 1 else x * factorial x - 1

in about 30s by changing the background and increasing the size limit.


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


Conjure, Copyright 2020  Rudy Matela,
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

[build-status]:                     https://travis-ci.org/rudymatela/conjure.svg?branch=master
[build-log]:                        https://travis-ci.org/rudymatela/conjure
[hackage-version]:                  https://img.shields.io/hackage/v/code-conjure.svg
[code-conjure-on-hackage]:          https://hackage.haskell.org/package/code-conjure
[stackage-lts-badge]:               https://stackage.org/package/code-conjure/badge/lts
[stackage-nightly-badge]:           https://stackage.org/package/code-conjure/badge/nightly
[code-conjure-on-stackage]:         https://stackage.org/package/code-conjure
[code-conjure-on-stackage-lts]:     https://stackage.org/lts/package/code-conjure
[code-conjure-on-stackage-nightly]: https://stackage.org/nightly/package/code-conjure
