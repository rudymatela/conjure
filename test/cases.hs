-- Copyright (C) 2021 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).

import Test
import Conjure.Cases
import Test.LeanCheck.Error (errorToLeft)

main :: IO ()
main  =  mainTest tests 5040

tests :: Int -> [Bool]
tests n  =
  [ True

  , length (cases (undefined :: ()))      == 1
  , length (cases (undefined :: Bool))    == 2
  , length (cases (undefined :: Int))     == 0
  , length (cases (undefined :: Integer)) == 0
  , length (cases (undefined :: [Int]))   == 2
  , length (cases (undefined :: [Bool]))  == 2

  , fvl sumFxpr (sumV :$ val [1,2,3,11::Int]) == (17 :: Int)
  , fvl sumFxpr (sumV :$ val [1,2,3::Int])    == ( 6 :: Int)
  , fvl sumFxpr (sumV :$ val [1,2,3,4::Int])  == (10 :: Int)

  , fvl factFxpr (factV :$ val (0 :: Int)) == (1 :: Int)
  , fvl factFxpr (factV :$ val (1 :: Int)) == (1 :: Int)
  , fvl factFxpr (factV :$ val (2 :: Int)) == (2 :: Int)
  , fvl factFxpr (factV :$ val (3 :: Int)) == (6 :: Int)
  , fvl factFxpr (factV :$ val (4 :: Int)) == (24 :: Int)
  , fvl factFxpr (factV :$ val (9 :: Int)) == (362880 :: Int)
  , errorToLeft (fvl factFxpr (factV :$ val (10 :: Int)))
    == Right (3628800 :: Int)
  , errorToLeft (fvl factFxpr (factV :$ val (11 :: Int)) == (39916800 :: Int))
    == Left "fxprToDynamic: recursion limit reached"

  , fvl isZeroFxpr (isZeroV :$ val (0 :: Int)) == True
  , fvl isZeroFxpr (isZeroV :$ val (1 :: Int)) == False

  , fvl nullFxpr (nullV :$ val [0,1,2,3::Int]) == False
  , fvl nullFxpr (nullV :$ val ([] :: [Int])) == False
  ]

fvl :: Typeable a => Fxpr -> Expr -> a
fvl  =  fevl exprExpr 12

sumV, factV, nullV, isZeroV :: Expr
factV    =  var "fact"   (undefined :: Int -> Int)
sumV     =  var "sum"    (undefined :: [Int] -> Int)
isZeroV  =  var "isZero" (undefined :: Int -> Bool)
nullV    =  var "null"   (undefined :: [Int] -> Bool)

-- in Conjure.Conjurable there should be a function
-- called conjureExprExpr that conjures this for the required types.
exprExpr :: Expr -> Expr
exprExpr e  =  evl $ headOr err $ mapMaybe ($$ e)
  [ value "expr" (expr :: Int -> Expr)
  , value "expr" (expr :: [Int] -> Expr)
  , value "expr" (expr :: Bool -> Expr)
  , value "expr" (expr :: [Bool] -> Expr)
  ]
  where
  err  =  error "exprExpr: unhandled type"

sumFxpr :: Fxpr
sumFxpr  =
  [ sum' nil           =-  zero
  , sum' (xx -:- xxs)  =-  xx -+- (sumV :$ xxs)
  ]
  where
  sum' e  =  sumV :$ e

factFxpr :: Fxpr
factFxpr  =
  [ fact' zero  =-  one
  , fact' xx    =-  xx -*- (factV :$ (xx -+- minusOne))
  ]
  where
  fact' e  =  factV :$ e

nullFxpr :: Fxpr
nullFxpr  =
  [ null' nil           =-  false
  , null' (xx -:- xxs)  =-  false
  ]
  where
  null' e  =  nullV :$ e

isZeroFxpr :: Fxpr
isZeroFxpr  =
  [ isZero' zero  =-  true
  , isZero' xx    =-  false
  ]
  where
  isZero' e  =  isZeroV :$ e

(=-) = (,)
infixr 0 =-
