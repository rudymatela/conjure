Conjure
=======

[![Conjure's Build Status][build-status]][build-log]
[![Conjure on Hackage][hackage-version]][code-conjure-on-hackage]
[![Conjure on Stackage LTS][stackage-lts-badge]][code-conjure-on-stackage-lts]
[![Conjure on Stackage Nightly][stackage-nightly-badge]][code-conjure-on-stackage-nightly]

![Conjure logo][conjure-logo]

Conjure is a tool that synthesizes Haskell functions out of partial definitions.


Installing
----------

To install the [latest Conjure version from Hackage], just run:

	$ cabal update
	$ cabal install code-conjure

Prerequisites are [express], [leancheck] and [speculate].
They should be automatically resolved and installed by [Cabal].

NOTE: the name of the Hackage package is __[`code-conjure`]__
-- not to be confused with [Conjure the BitTorrent client].

Starting from Cabal v3.0, you need to pass `--lib` as an argument to
`cabal install` to install packages globally on the `default` user environment:

	$ cabal install code-conjure --lib

If you already have Conjure installed
[Cabal may refuse to update](https://github.com/haskell/cabal/issues/7373)
to the latest version.
To update, you need to reset your user's cabal installation with:

	rm -rf ~/.cabal/{bin,lib,logs,share,store} ~/.ghc/*/

WARNING: the above command will erase all user-local packages.


Synthesizing functions
----------------------

To use Conjure, import the library with:

	import Conjure

Then, declare a partial definition of a function to be synthesized.
For example,
here is a partial implementation of a function that computes the factorial of a number:

	factorial :: Int -> Int
	factorial 1  =  1
	factorial 2  =  2
	factorial 3  =  6
	factorial 4  =  24

Next, declare a list of ingredients that seem like interesting pieces
in the final fully-defined implementation.
For example,
here is a list of ingredients including
addition, multiplication, subtraction and their neutral elements:

	ingredients :: [Ingredient]
	ingredients  =  [ con (0::Int)
	                , con (1::Int)
	                , fun "+" ((+) :: Int -> Int -> Int)
	                , fun "*" ((*) :: Int -> Int -> Int)
	                , fun "-" ((-) :: Int -> Int -> Int)
	                ]

Finally, call the [`conjure`] function,
passing the function name, the partial definition and the list of ingredients:

	factorial :: Int -> Int
	-- 0.1s, testing 4 combinations of argument values
	-- 0.8s, pruning with 27/65 rules
	-- 0.8s, 3 candidates of size 1
	-- 0.9s, 3 candidates of size 2
	-- 0.9s, 7 candidates of size 3
	-- 0.9s, 8 candidates of size 4
	-- 0.9s, 28 candidates of size 5
	-- 0.9s, 35 candidates of size 6
	-- 0.9s, 167 candidates of size 7
	-- 0.9s, tested 95 candidates
	factorial 0  =  1
	factorial x  =  x * factorial (x - 1)

Conjure is able to synthesize the above implementation in less than a second
in a regular laptop computer.

It is also possible to generate a folding implementation
like the following:

	factorial x  =  foldr (*) 1 [1..x]

by including [`enumFromTo`] and [`foldr`] in the background.

For more information, see the `eg/factorial.hs` example and
the Haddock documentation for the [`conjure`] function.


Synthesizing functions over algebraic data types
------------------------------------------------

Conjure is not limited to integers,
it works for functions over algebraic data types too.
Consider the following partial definition of `take`:

	take' :: Int -> [a] -> [a]
	take' 0 [x]    =  []
	take' 1 [x]    =  [x]
	take' 0 [x,y]  =  []
	take' 1 [x,y]  =  [x]
	take' 2 [x,y]  =  [x,y]
	take' 3 [x,y]  =  [x,y]

Conjure is able to find an appropriate implementation
given list constructors, zero, one and subtraction:

	> conjure "take" (take' :: Int -> [A] -> [A])
	>   [ con (0 :: Int)
	>   , con (1 :: Int)
	>   , con ([] :: [A])
	>   , fun ":" ((:) :: A -> [A] -> [A])
	>   , fun "-" ((-) :: Int -> Int -> Int)
	>   ]
	take :: Int -> [A] -> [A]
	-- 0.2s, testing 153 combinations of argument values
	-- 0.2s, pruning with 4/7 rules
	-- ...   ...   ...   ...   ...
	-- 0.4s, 5 candidates of size 9
	-- 0.4s, tested 15 candidates
	take 0 xs  =  []
	take x []  =  []
	take x (y:xs)  =  y:take (x - 1) xs

The above example also takes less than a second to run in a modern laptop.
The selection of functions in the list of ingredients was minimized
to what was absolutely needed here.
With a larger collection of ingredients YMMV.


Synthesizing from specifications (for advanced users)
-----------------------------------------------------

Conjure also supports synthesizing from a functional specification
with [`conjureFromSpec`].

Consider a function `duplicates` that given a list of values
should return all values that are repeated.
The resulting list should itself not contain repetitions.

Even an experienced programmer
may take a few minutes to come up with a correct definition for `duplicates`
even when told that a conditional definition
is possible using only
`[]`,
`:`,
`not`,
`&&` and
`elem`.
(We invite the reader to try.)

We can encode a specification of duplicates with test properties like so:

	duplicatesSpec :: ([Int] -> [Int]) -> [Property]
	duplicatesSpec duplicates  =  and
	  [ property $ \x xs -> (count (x ==) xs > 1) == elem x (duplicates xs)
	  , property $ \x xs -> count (x ==) (duplicates xs) <= 1
	  ]  where  count p  =  length . filter p

Conjure finds a solution in 1 second
with the following call:

	conjureFromSpec "duplicates" duplicatesSpec
	  [ con ([] :: [Int])
	  , fun "not" not
	  , fun "&&" (&&)
	  , fun ":" ((:) :: Int -> [Int] -> [Int])
	  , fun "elem" (elem :: Int -> [Int] -> Bool)
	  , guard  -- allows guards
	  ]

This is the definition produced by Conjure:

	duplicates []  =  []
	-- 0.2s, pruning with 21/26 rules
	-- 0.2s, 2 candidates of size 1
	-- 0.3s, 1 candidates of size 2
	-- 0.3s, 0 candidates of size 3
	-- 0.3s, 2 candidates of size 4
	-- 0.3s, 1 candidates of size 5
	-- 0.3s, 2 candidates of size 6
	-- 0.3s, 3 candidates of size 7
	-- 0.3s, 8 candidates of size 8
	-- 0.3s, 13 candidates of size 9
	-- 0.3s, 18 candidates of size 10
	-- 0.3s, 21 candidates of size 11
	-- 0.3s, 28 candidates of size 12
	-- 0.3s, 39 candidates of size 13
	-- 0.4s, 54 candidates of size 14
	-- 0.5s, 67 candidates of size 15
	-- 0.7s, 80 candidates of size 16
	-- 1.0s, 99 candidates of size 17
	-- 1.0s, tested 340 candidates
	duplicates (x:xs)
	  | elem x xs && not (elem x (duplicates xs))  =  x:duplicates xs
	  | otherwise  =  duplicates xs


Related work
------------

__Conjure's dependencies__.
Internally, Conjure uses [LeanCheck], [Speculate] and [Express].
[LeanCheck] does testing similarly to [QuickCheck], [SmallCheck] or [Feat].
[Speculate] discovers equations similarly to [QuickSpec].
[Express] encodes expressions involving [Dynamic] types.

[Speculate]:  https://github.com/rudymatela/speculate
[Express]:    https://github.com/rudymatela/express
[QuickCheck]: https://hackage.haskell.org/package/QuickCheck
[smallcheck]: https://hackage.haskell.org/package/smallcheck
[Feat]:       https://hackage.haskell.org/package/testing-feat
[QuickSpec]:  https://hackage.haskell.org/package/quickspec
[Dynamic]:    https://hackage.haskell.org/package/base/docs/Data-Dynamic.html


__Program synthesis within Haskell.__

[MagicHaskeller]: https://hackage.haskell.org/package/MagicHaskeller
[Igor II]: https://cogsys.uni-bamberg.de/projects/effalip

[MagicHaskeller] (2007) is another tool
that is able to generate Haskell code automatically.
It supports recursion through
catamorphisms, paramorphisms and the [`fix`] function.
[Igor II] (2010) is able to synthesize Haskell
programs as well.

[Hoogle]: https://hoogle.haskell.org/
[Hoogle+]: https://hoogleplus.goto.ucsd.edu/

[Hoogle] (2004) is a search engine for Haskell functions.
It is not able to synthesize expressions
but it can find functions that match a type.
[Hoogle+] (2020) is similar to Hoogle
but is able to search for small expressions.
In addition to the type, Hoogle+ allows
users to provide tests that the function should pass.


__Program synthesis beyond Haskell.__

[PushGP]: https://github.com/lspector/Clojush
[G3P]: https://github.com/t-h-e/HeuristicLab.CFGGP

[PushGP] (2002) and [G3P] (2017) are genetic programming systems
that are able to synthesize programs in Push and Python respectively.
Differently from Conjure or MagicHaskeller,
they require around a hundred tests for traning
instead of just about half a dozen.

[Barliman]:       https://github.com/webyrd/Barliman

[Barliman] (2016) for Lisp is another tool that does program synthesis.


Further reading
---------------

For a detailed documentation of each function, see
[Conjure's Haddock documentation].

The `eg` folder in the source distribution
contains more than 60 examples of use.


Conjure, Copyright 2021-2025  Rudy Matela,
distribued under the 3-clause BSD license.


[Conjure's Haddock documentation]: https://hackage.haskell.org/package/code-conjure/docs/Conjure.html
[`conjure`]:             https://hackage.haskell.org/package/code-conjure/docs/Conjure.html#v:conjure
[`conjureFromSpec`]:     https://hackage.haskell.org/package/code-conjure/docs/Conjure.html#v:conjureFromSpec

[`foldr`]:               https://hackage.haskell.org/package/base/docs/Prelude.html#v:foldr
[`enumFromTo`]:          https://hackage.haskell.org/package/base/docs/Prelude.html#v:enumFromTo
[`fix`]:                 https://hackage.haskell.org/package/base/docs/Data-Function.html#v:fix
[`holds`]:               https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html#v:holds

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
