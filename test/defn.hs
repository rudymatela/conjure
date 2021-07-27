-- Copyright (C) 2021 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).

import Test
import Conjure.Defn
import Test.LeanCheck.Error (errorToLeft)

main :: IO ()
main  =  mainTest tests 5040

tests :: Int -> [Bool]
tests n  =
  [ True

  , dvl sumDefn (sumV :$ val [1,2,3,11::Int]) == (17 :: Int)
  , dvl sumDefn (sumV :$ val [1,2,3::Int])    == ( 6 :: Int)
  , dvl sumDefn (sumV :$ val [1,2,3,4::Int])  == (10 :: Int)

  , dvl andDefn (andV :$ val [False,True])      == False
  , dvl andDefn (andV :$ val [True,True])       == True
  , dvl andDefn (andV :$ val [True,False,True]) == False

  , dvl factDefn (factV :$ val (0 :: Int)) == (1 :: Int)
  , dvl factDefn (factV :$ val (1 :: Int)) == (1 :: Int)
  , dvl factDefn (factV :$ val (2 :: Int)) == (2 :: Int)
  , dvl factDefn (factV :$ val (3 :: Int)) == (6 :: Int)
  , dvl factDefn (factV :$ val (4 :: Int)) == (24 :: Int)
  , dvl factDefn (factV :$ val (9 :: Int)) == (362880 :: Int)
  , errorToLeft (dvl factDefn (factV :$ val (10 :: Int)))
    == Right (3628800 :: Int)
  , errorToLeft (dvl factDefn (factV :$ val (11 :: Int)) == (39916800 :: Int))
    == Left "toDynamicWithDefn: recursion limit reached"

  , dvl isZeroDefn (isZeroV :$ val (0 :: Int)) == True
  , dvl isZeroDefn (isZeroV :$ val (1 :: Int)) == False

  , dvl nullDefn (nullV :$ val [0,1,2,3::Int]) == False
  , dvl nullDefn (nullV :$ val ([] :: [Int])) == False
  ]

dvl :: Typeable a => Defn -> Expr -> a
dvl  =  devl exprExpr 12

sumV, factV, nullV, isZeroV :: Expr
factV    =  var "fact"   (undefined :: Int -> Int)
sumV     =  var "sum"    (undefined :: [Int] -> Int)
andV     =  var "and"    (undefined :: [Bool] -> Bool)
isZeroV  =  var "isZero" (undefined :: Int -> Bool)
nullV    =  var "null"   (undefined :: [Int] -> Bool)

-- NOTE: a hack for testing needs all types that are Express as arguments of
--       undefined.
exprExpr :: Expr -> Expr
exprExpr  =  conjureExpress (undefined :: Bool -> [Bool] -> Int -> [Int] -> ())

sumDefn :: Defn
sumDefn  =  [ sum' nil           =-  zero
            , sum' (xx -:- xxs)  =-  xx -+- (sumV :$ xxs)
            ]  where  sum' e  =  sumV :$ e

andDefn :: Defn
andDefn  =  [ and' nilBool       =-  true
            , and' (pp -:- pps)  =-  pp -&&- (andV :$ pps)
            ]  where  and' e  =  andV :$ e

factDefn :: Defn
factDefn  =  [ fact' zero  =-  one
             , fact' xx    =-  xx -*- (factV :$ (xx -+- minusOne))
             ]  where  fact' e  =  factV :$ e

nullDefn :: Defn
nullDefn  =  [ null' nil           =-  false
             , null' (xx -:- xxs)  =-  false
             ]  where  null' e  =  nullV :$ e

isZeroDefn :: Defn
isZeroDefn  =  [ isZero' zero  =-  true
               , isZero' xx    =-  false
               ]  where  isZero' e  =  isZeroV :$ e

(=-) = (,)
infixr 0 =-
