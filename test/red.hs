-- Copyright (C) 2021-2024 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).

import Test
import Conjure.Red
import Data.Dynamic

main :: IO ()
main  =  mainTest tests 5040

tests :: Int -> [Bool]
tests n  =
  [ True

  -- tests of conjureIsDeconstruction --

  -- obvious deconstructions
  , isDecon (minus :$ i_ :$ one) == True
  , isDecon (minus :$ i_ :$ two) == True
  , isDecon (div' i_ two) == True
  , isDecon (tail' is_) == True
  , isDecon (init' is_) == True
  , isDecon (drop' one is_) == True

  -- obvious constructions
  , isDecon (i_ -+- one) == False
  , isDecon (i_ -+- two) == False
  , isDecon (i_ -*- two)  == False
  , isDecon (xx -:- is_) == False
  , isDecon (is_ -++- unit xx) == False

  -- doing nothing is not deconstructing
  , isDecon (i_) == False
  , isDecon (is_) == False

  -- double deconstructions & constructions
  , isDecon (minusOne -+- i_) == True
  , isDecon (minus :$ (minus :$ i_ :$ one) :$ one) == True
  , isDecon (minus :$ i_ :$ three) == True
  , isDecon (minus :$ i_ :$ four) == True
  , isDecon (minus :$ i_ :$ five) == True
  , isDecon (minus :$ i_ :$ six) == True
  , isDecon (drop' two is_) == True
  , isDecon (drop' three is_) == True
  , isDecon (drop' xx is_) == False -- may not deconstruct!

  , isDecon (tail' (tail' is_)) == False -- does not deconstruct [1]
  , isDecon (init' (init' is_)) == False -- does not deconstruct [1]
  , isDecon (init' (tail' is_)) == False -- does not deconstruct [1]

  , isDecon (drop' one (drop' one is_)) == True
  , isDecon (drop' one (tail' is_)) == True

  , isDecon (take' one is_) == False -- does not deconstruct [1]
  , isDecon (take' two is_) == False -- does not deconstruct [1,1]

  -- counter-intuitive but true: x `mod` y is a deconstruction of y:
  -- x `mod` y < y  for  y > 0
  , isDecon (mod' xx i_) == True

  , isDecon (mod' i_ two) == False -- does not deconstruct 1
  , isDecon (mod' i_ xx)  == False -- may not deconstruct 1
  , isDecon (div' xx yy)  == False -- must have a hole to indicate the value being deconstructed
  , isDecon (div' i_ i_)  == False -- two holes are not allowed
  , isDecon (head' is_)   == False -- must deconstruct to the same type

  -- constant "deconstructions"
  , isDecon (const' zero i_) == False -- always mapping to size 0 is not allowed!
  , isDecon (const' nil is_) == False -- always mapping to size 0 is not allowed!
  , isDecon (const' one i_)  == False -- does not deconstruct 1

  -- negative "deconstructions"
  , isDecon (minus :$ zero :$ i_) == False
  , isDecon (minus :$ one :$ i_)  == False

  -- boolean "deconstructions"
  , isDecon (not' b_) == False -- always mapping to size 0 is not allowed!
  , isDecon (false -||- b_) == False -- always mapping to size 0 is not allowed!

  , candidateDeconstructionsFrom (div' xx yy) == [ div' i_ yy
                                                 , div' xx i_
                                                 ]
  , candidateDeconstructionsFrom (div' xx xx) == []
  , candidateDeconstructionsFrom ((xx -+- xx) -+- yy) == [(xx -+- xx) -+- i_]

  , candidateDeconstructionsFromHoled (div' i_ i_) == [ div' i_ xx
                                                      , div' xx i_
                                                      ]
  , candidateDeconstructionsFromHoled (div' xx yy) == []
  , candidateDeconstructionsFromHoled ((i_ -+- i_) -+- i_) ==
      [ (i_ -+- xx) -+- yy
      , (i_ -+- xx) -+- xx
      , (xx -+- i_) -+- yy
      , (xx -+- i_) -+- xx
      , (xx -+- yy) -+- i_
      , (xx -+- xx) -+- i_
      ]

  , argumentSubsets (ff xx) (ff (xx -+- one)) == [[(xx,xx -+- one)]]
  , argumentSubsets (ff xx) (ff yy) == []

  , argumentSubsets (xx -?- yy) (yy -?- xx) == [[(xx,xx),(yy,yy)]]

  , argumentSubsets (xx -:- xxs) (yy -:- yys) == []

  , argumentSubsets ((xx,yy) --..- zz) ((xx,zz) --..- yy)
    == [ [(xx,xx)]
       , [(yy,yy), (zz,zz)]
       ]

  , argumentSubsets ((xx,yy) --..- zz) ((zz,xx) --..- yy)
    == [[(xx,xx), (yy,yy), (zz,zz)]]

  , argumentSubsets ((xx,yy) --..- zz) ((dec zz,xx) --..- yy)
    == [[(xx,xx), (yy,yy), (zz,dec zz)]]

  -- simple integer descent
  , descends isDecOf (ff xx) (ff xx) == False
  , descends isDecOf (ff xx) (ff (xx -+- one)) == False
  , descends isDecOf (ff xx) (ff (dec xx)) == True
  , descends isDecOf (ff xx) (ff (yy `mod'` xx)) == True

  -- simple list descent
  , descends isDecOf (ff xxs) (ff xxs) == False
  , descends isDecOf (ff xxs) (ff (tail' xxs)) == True
  , descends isDecOf (ff (xx -:- xxs)) (ff xxs) == True
  , descends isDecOf (ff xxs) (ff (xxs -++- xxs)) == False

  -- double list descent
  , descends isDecOf (xxs -++- yys) (xxs -++- yys) == False
  , descends isDecOf (xxs -++- yys) (xxs -++- tail' yys) == True

  , descends isDecOf (xxs -++- yys) (tail' yys -++- yys) == False
  , descends isDecOf (xxs -++- yys) ((xx -:- xxs) -++- tail' yys) == True

  , descends isDecOf (xxs -++- yys) (head' xxs -:- tail' xxs  -++-  head' yys -:- tail' yys) == False

  -- gcd descent
  , descends isDecOf (ff2 xx yy) (ff2 yy (xx `mod'` yy)) == True -- actual gcd descent
  , descends isDecOf (ff2 xx yy) (ff2 yy (yy `mod'` yy)) == True -- other

  -- interleave descent
  , descends isDecOf (xxs -\/- yys) (yys -\/- tail' xxs) == False -- TODO: should be True
  , descends isDecOf (xxs -\/- yys) (tail' yys -\/- xxs) == False -- TODO: should be True
  , descends isDecOf (xxs -\/- yys) (tail' yys -\/- tail' xxs) == False -- TODO: should be True
  , descends isDecOf ((xx -:- xxs) -\/- yys) (yys -\/- xxs) == True
  , descends isDecOf (xxs -\/- (yy -:- yys)) (yys -\/- xxs) == True
  , descends isDecOf ((xx -:- xxs) -\/- (yy -:- yys)) (yys -\/- xxs) == True
  ]

isDecOf :: Expr -> Expr -> Bool
e1 `isDecOf` e2  =  any ((e1 -|- e2) `isInstanceOf`)
  [ tail' xxs -|- xxs
  , dec xx -|- xx
  , yy `mod'` xx -|- xx
  ]

(-\/-) :: Expr -> Expr -> Expr
exs -\/- eys  =  interleaveE :$ exs :$ eys
  where
  interleaveE  =  value "\\/" ((\/) :: [Int] -> [Int] -> [Int])
  [] \/ ys  =  ys
  (x:xs) \/ ys  =  x : (ys \/ xs)

isDecon :: Expr -> Bool
isDecon =  conjureIsDeconstruction (undefined :: [Int] -> [Char] -> [Bool]) 60

dec :: Expr -> Expr
dec ex  =  minus :$ ex :$ one
