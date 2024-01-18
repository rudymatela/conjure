-- Copyright (C) 2021 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).

import Test
import Conjure.Defn
import Test.LeanCheck.Error (errorToLeft)
import Data.Express.Fixtures

main :: IO ()
main  =  mainTest tests 5040

tests :: Int -> [Bool]
tests n  =
  [ True

  , dvl sumDefn (sumV :$ val [1,2,3,11::Int]) == (17 :: Int)
  , dvl sumDefn (sumV :$ val [1,2,3::Int])    == ( 6 :: Int)
  , dvl sumDefn (sumV :$ val [1,2,3,4::Int])  == (10 :: Int)

  , dvl andDefn (andV :$ val [False,False])     == False
  , dvl andDefn (andV :$ val [False,True])      == False
  , dvl andDefn (andV :$ val [True,True])       == True
  , dvl andDefn (andV :$ val [True,False,True]) == False
  , dvl orDefn  (orV  :$ val [False,False])     == False
  , dvl orDefn  (orV  :$ val [False,True])      == True
  , dvl orDefn  (orV  :$ val [True,True])       == True
  , dvl orDefn  (orV  :$ val [True,False,True]) == True

  , dvl and1Defn (andV :$ val [False,False])     == False
  , dvl and1Defn (andV :$ val [False,True])      == False
  , dvl and1Defn (andV :$ val [True,True])       == True
  , dvl and1Defn (andV :$ val [True,False,True]) == False
  , dvl or1Defn  (orV  :$ val [False,False])     == False
  , dvl or1Defn  (orV  :$ val [False,True])      == True
  , dvl or1Defn  (orV  :$ val [True,True])       == True
  , dvl or1Defn  (orV  :$ val [True,False,True]) == True

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

  , dvl fact1Defn (factV :$ val (0 :: Int)) == (1 :: Int)
  , dvl fact1Defn (factV :$ val (1 :: Int)) == (1 :: Int)
  , dvl fact1Defn (factV :$ val (2 :: Int)) == (2 :: Int)
  , dvl fact1Defn (factV :$ val (3 :: Int)) == (6 :: Int)
  , dvl fact1Defn (factV :$ val (4 :: Int)) == (24 :: Int)
  , dvl fact1Defn (factV :$ val (9 :: Int)) == (362880 :: Int)
  , errorToLeft (dvl fact1Defn (factV :$ val (10 :: Int)))
    == Right (3628800 :: Int)
  , errorToLeft (dvl fact1Defn (factV :$ val (11 :: Int)) == (39916800 :: Int))
    == Left "toDynamicWithDefn: recursion limit reached"

  , dvl isZeroDefn (isZeroV :$ val (0 :: Int)) == True
  , dvl isZeroDefn (isZeroV :$ val (1 :: Int)) == False
  , dvl isOneDefn  (isOneV  :$ val (0 :: Int)) == False
  , dvl isOneDefn  (isOneV  :$ val (1 :: Int)) == True

  , dvl nullDefn (nullV :$ val [0,1,2,3::Int]) == False
  , dvl nullDefn (nullV :$ val ([] :: [Int]))  == True

  , holds n $ cevl 60 sumDefn    === (sum :: [Int] -> Int)
  , holds n $ cevl 60 andDefn    === (and :: [Bool] -> Bool)
  , holds n $ cevl 60 orDefn     === (or :: [Bool] -> Bool)
  , holds n $ cevl 60 isZeroDefn === ((==0) :: Int -> Bool)
  , holds n $ cevl 60 isOneDefn  === ((==1) :: Int -> Bool)
  , holds n $ cevl 60 nullDefn   === (null :: [Int] -> Bool)
  , holds n $ cevl 60 appendDefn ==== ((++) :: [Int] -> [Int] -> [Int])

  -- evaluating at the incorrect types should return Nothing
  , isNothing (cevaluate 60 sumDefn :: Maybe ([Bool] -> Bool))
  , isNothing (cevaluate 60 andDefn :: Maybe ([Int] -> Int))
  , isNothing (cevaluate 60 nullDefn :: Maybe ([Int] -> Int))

  , isRedundantDefn sumDefn    == False
  , isRedundantDefn factDefn   == False
  , isRedundantDefn fact1Defn  == False
  , isRedundantDefn nullDefn   == False
  , isRedundantDefn isZeroDefn == False
  , isRedundantDefn isOneDefn  == False
  , isRedundantDefn andDefn    == False
  , isRedundantDefn orDefn     == False
  , isRedundantDefn and1Defn   == False
  , isRedundantDefn or1Defn    == False
  , isRedundantDefn appendDefn == False

  , isRedundantDefn const0Defn == False
  , isRedundantDefn const0RedundantDefn == False -- TODO: detect this redundancy!

  , isRedundantDefn redundantDefn1
  , isRedundantDefn canonicalDefn1 == False
  , isRedundantDefn redundantDefn2
  ]

dvl :: Typeable a => Defn -> Expr -> a
dvl  =  devl exprExpr 11

sumV, factV, nullV, isZeroV :: Expr
factV    =  var "fact"   (undefined :: Int -> Int)
sumV     =  var "sum"    (undefined :: [Int] -> Int)
andV     =  var "and"    (undefined :: [Bool] -> Bool)
orV      =  var "or"     (undefined :: [Bool] -> Bool)
isZeroV  =  var "isZero" (undefined :: Int -> Bool)
isOneV   =  var "isOne"  (undefined :: Int -> Bool)
nullV    =  var "null"   (undefined :: [Int] -> Bool)
appendV  =  var "++"     (undefined :: [Int] -> [Int] -> [Int])

-- NOTE: a hack for testing needs all types that are Express as arguments of
--       undefined.
exprExpr :: Expr -> Expr
exprExpr  =  conjureExpress (undefined :: Bool -> [Bool] -> Int -> [Int] -> ())

sumDefn :: Defn
sumDefn  =  [ sum' nil           =-  zero
            , sum' (xx -:- xxs)  =-  xx -+- (sumV :$ xxs)
            ]  where  sum' e  =  sumV :$ e

factDefn :: Defn
factDefn  =  [ fact' zero  =-  one
             , fact' xx    =-  xx -*- (factV :$ (xx -+- minusOne))
             ]  where  fact' e  =  factV :$ e

fact1Defn :: Defn
fact1Defn  =  [ fact' xx  =-  if' (xx -==- zero) (one) (xx -*- (factV :$ (minus :$ xx :$ one)))
              ]  where  fact' e  =  factV :$ e

nullDefn :: Defn
nullDefn  =  [ null' nil           =-  true
             , null' (xx -:- xxs)  =-  false
             ]  where  null' e  =  nullV :$ e

isZeroDefn :: Defn
isZeroDefn  =  [ isZero' zero  =-  true
               , isZero' xx    =-  false
               ]  where  isZero' e  =  isZeroV :$ e

isOneDefn :: Defn
isOneDefn  =  [ isOne' xx  =-  xx -==- one ]
  where isOne' e  =  isOneV :$ e

andDefn :: Defn
andDefn  =  [ and' nilBool       =-  true
            , and' (pp -:- pps)  =-  pp -&&- (andV :$ pps)
            ]  where  and' e  =  andV :$ e

orDefn :: Defn
orDefn  =  [ or' nilBool       =-  false
           , or' (pp -:- pps)  =-  pp -||- (orV :$ pps)
           ]  where or' e  =  orV :$ e

and1Defn :: Defn
and1Defn  =  [ and' pps  =-  null' pps -||- head' pps -&&- and' (tail' pps)
             ]  where  and' e  =  andV :$ e

or1Defn :: Defn
or1Defn  =  [ or' pps  =-  not' (null' pps) -&&- (head' pps -||- or' (tail' pps))
            ]  where or' e  =  orV :$ e

appendDefn :: Defn
appendDefn  =  [ nil -++- xxs  =-  xxs
               , (xx -:- xxs) -++- yys  =-  xx -:- (xxs -++- yys)
               ]  where  exs -++- eys  =  appendV :$ exs :$ eys

const0Defn :: Defn
const0Defn  =  [ xx  =-  zero ]

const0RedundantDefn :: Defn
const0RedundantDefn  =  [ zero  =-  zero
                        , xx    =-  zero
                        ]

-- Here is an example of a redundant 'Defn':
--
-- > 0 ? 0  =  1
-- > 0 ? x  =  1
-- > x ? 0  =  x
-- > x ? y  =  x
redundantDefn1 :: Defn
redundantDefn1  =  [ zero -?- zero  =-  one
                   , zero -?- xx    =-  one
                   , xx   -?- zero  =-  xx
                   , xx   -?- yy    =-  xx
                   ]

-- The above is redundant because it is equivalent to:
--
-- > 0 ? _  =  1
-- > x ? _  =  x
canonicalDefn1 :: Defn
canonicalDefn1  =  [ zero -?- xx  =-  one
                   , xx   -?- yy  =-  xx
                   ]

-- same as redundantDefn1 but with arguments swapped
-- (mind the alpha-renaming)
redundantDefn2 :: Defn
redundantDefn2  =  [ zero -?- zero  =-  one
                   , zero -?- xx    =-  xx
                   , xx   -?- zero  =-  one
                   , xx   -?- yy    =-  yy
                   ]

(=-) = (,)
infixr 0 =-
