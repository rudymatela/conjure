-- Copyright (C) 2021-2025 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).

import Test
import Conjure.Defn
import Conjure.Defn.Redundancy
import Test.LeanCheck.Error (errorToLeft)
import Test.LeanCheck.Function ()
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
    == Left "Conjure.Defn.toDynamicWithDefn: recursion limit reached"

  , dvl fact1Defn (factV :$ val (0 :: Int)) == (1 :: Int)
  , dvl fact1Defn (factV :$ val (1 :: Int)) == (1 :: Int)
  , dvl fact1Defn (factV :$ val (2 :: Int)) == (2 :: Int)
  , dvl fact1Defn (factV :$ val (3 :: Int)) == (6 :: Int)
  , dvl fact1Defn (factV :$ val (4 :: Int)) == (24 :: Int)
  , dvl fact1Defn (factV :$ val (9 :: Int)) == (362880 :: Int)
  , errorToLeft (dvl fact1Defn (factV :$ val (10 :: Int)))
    == Right (3628800 :: Int)
  , errorToLeft (dvl fact1Defn (factV :$ val (11 :: Int)) == (39916800 :: Int))
    == Left "Conjure.Defn.toDynamicWithDefn: recursion limit reached"

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

  , holds n $ cevl 60 idDefn      === (id :: Int -> Int)
  , holds n $ cevl 60 constDefn   ==== (const :: Int -> Int -> Int)
  , holds n $ cevl 60 appDefn     ==== (($) :: (Int->Int) -> Int -> Int)
  , holds n $ \f g x -> cevl 60 composeDefn f g x == ((.) :: (Int->Int) -> (Int->Int) -> Int->Int) f g x

  -- evaluating at the incorrect types should return Nothing
  , isNothing (cevaluate 60 sumDefn :: Maybe ([Bool] -> Bool))
  , isNothing (cevaluate 60 andDefn :: Maybe ([Int] -> Int))
  , isNothing (cevaluate 60 nullDefn :: Maybe ([Int] -> Int))

  , noUnbound (xx -:- xxs, xxs)
  , hasUnbound (xx -:- xxs, yys)
  , isDefined sumDefn
  , isDefined andDefn
  , isDefined nullDefn
  , isDefined [(nil, nil), (xx -:- xxs, xxs)]
  , isUndefined [(nil, xxs), (xx -:- xxs, yys)]

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
  , isRedundantDefn const0RedundantDefn

  , isRedundantDefn constDefn == False
  , isRedundantDefn constRedundantDefn
  , isRedundantDefn constRedundantRedundantDefn

  , isRedundantDefn redundantDefn1
  , isRedundantDefn canonicalDefn1 == False
  , isRedundantDefn redundantDefn2

  , isRedundantDefn constBoolRedundantDefn
  , isRedundantDefn idListRedundantDefn == False
    -- NOTE: ^ ByIntroduction is not an active check
    --         as it does not pay-off in terms of performance

  , isRedundantBySubsumption  constRedundantDefn
  , isRedundantByRepetition   constRedundantDefn == False
  , isRedundantByIntroduction constRedundantDefn

  , isRedundantBySubsumption  constBoolRedundantDefn == False
  , isRedundantByRepetition   constBoolRedundantDefn
  , isRedundantByIntroduction constBoolRedundantDefn

  , isRedundantBySubsumption  idListRedundantDefn == False
  , isRedundantByRepetition   idListRedundantDefn == False
  , isRedundantByIntroduction idListRedundantDefn

  , isRedundantBySubsumption  constNilRedundantDefn == False
  , isRedundantByRepetition   constNilRedundantDefn
  , isRedundantByIntroduction constNilRedundantDefn

  , isRedundantBySubsumption  constFalseRedundantDefn == False
  , isRedundantByRepetition   constFalseRedundantDefn
  , isRedundantByIntroduction constFalseRedundantDefn == True

  , canonicalizeBndn (introduceVariableAt 2 (const' xxs (yy -:- yys), (yy -:- yys) -++- (yy -:- yys)))
    == (const' xxs yys, yys -++- yys)

  , canonicalizeBndnLast 1 (introduceVariableAt 2 (const' xxs (yy -:- yys), (yy -:- yys) -++- (yy -:- yys)))
    == (const' zzs yys, yys -++- yys)

  , canonicalizeBndnLast 2 (introduceVariableAt 2 (const' xxs (yy -:- yys), (yy -:- yys) -++- (yy -:- yys)))
    == (const' xxs zzs, zzs -++- zzs)

  ,       isBaseCase      (ff (xx -:- nil), xx)
  , not $ isBaseCase      (ff (xx -:- xxs), ff xxs)
  ,       isRecursiveCase (ff (xx -:- xxs), ff xxs)
  , not $ isRecursiveCase (ff (xx -:- nil), xx)

  , subsumedWith (// [(zero -+- zero, zero)]) (ff zero, zero) (ff xx, xx -+- xx) == True
  , subsumedWith (// [(zero -+- zero, zero)]) (ff zero, zero) (ff xx, xx -+- zero) == True
  , subsumedWith (// [(zero -+- zero, zero)]) (ff zero, zero) (ff xx, xx -+- one) == False
  , subsumedWith (// [(zero -+- zero, zero)]) (zero -?- xx, zero) (xx -?- yy, xx -+- xx) == True
  , subsumedWith (// [(zero -+- zero, zero)]) (zero -?- xx, zero) (xx -?- yy, xx -+- zero) == True
  , subsumedWith (// [(zero -+- zero, zero)]) (zero -?- xx, zero) (xx -?- yy, xx -+- one) == False
  , subsumedWith (// [(zero -+- zero, zero)]) (zero -?- xx, zero) (xx -?- yy, xx -+- yy) == False

  , isRedundantModuloRewriting  (// [(zero -+- zero, zero)]) [(ff zero, zero),(ff xx, xx -+- xx)] == True
  , isRedundantModuloRewriting  (// [(zero -+- zero, zero)]) [(ff zero, zero),(ff xx, xx -+- zero)] == True
  , isRedundantModuloRewriting  (// [(zero -+- zero, zero)]) [(ff zero, zero),(ff xx, xx -+- one)] == False
  , isRedundantModuloRewriting  (// [(zero -+- zero, zero)]) [(zero -?- xx, zero),(xx -?- yy, xx -+- xx)] == True
  , isRedundantModuloRewriting  (// [(zero -+- zero, zero)]) [(zero -?- xx, zero),(xx -?- yy, xx -+- zero)] == True
  , isRedundantModuloRewriting  (// [(zero -+- zero, zero)]) [(zero -?- xx, zero),(xx -?- yy, xx -+- one)] == False
  , isRedundantModuloRewriting  (// [(zero -+- zero, zero)]) [(zero -?- xx, zero),(xx -?- yy, xx -+- yy)] == False

  , isRedundantModuloRewriting (// [(xx -+- zero, xx)])
      [ (zero -?- xx, xx)
      , (xx -?- zero, xx)
      , (xx -?- yy, xx -+- xx)
      ] == False

  , subsumedWith (// [(xx -+- zero, xx)]) (xx -?- zero, xx) (xx -?- yy, xx -+- yy) == True

  , isRedundantModuloRewriting (// [(xx -+- zero, xx)])
      [ (zero -?- xx, xx)
      , (xx -?- zero, xx)
      , (xx -?- yy, xx -+- yy)
      ] == True

  , showDefn factDefn
    == "fact 0  =  1\n"
    ++ "fact x  =  x * fact (x - 1)\n"

  , showDefn fact1Defn
    == "fact x  =  if x == 0\n"
    ++ "           then 1\n"
    ++ "           else x * fact (x - 1)\n"

  , showDefn factCDefn
    == "fact x  =  case x == 0 of\n"
    ++ "           False -> x * fact (x - 1)\n"
    ++ "           True  -> 1\n"

  , showDefn sumDefn
    == "sum []  =  0\n"
    ++ "sum (x:xs)  =  x + sum xs\n"

  , showDefn appendDefn
    == "[] ++ xs  =  xs\n"
    ++ "(x:xs) ++ ys  =  x:(xs ++ ys)\n"

  , showDefn isZeroCDefn
    == "isZero x  =  case compare x 0 of\n"
    ++ "             LT -> False\n"
    ++ "             EQ -> True\n"
    ++ "             GT -> False\n"

  , caneta (ff xx, zero) == False
  , caneta (ff xx, gg xx) == True
  , caneta (ff xx, one -+- two) == False
  , caneta (ff xx, one -+- xx) == False -- we don't eta-reduce infix ops
  , caneta (ff xx, xx -+- one) == False
  , caneta (ff2 xx yy, xx -+- yy) == False -- we don't eta-reduce infix ops
  , caneta (ff2 xx yy, one -+- yy) == False -- we don't eta-reduce infix ops
  , caneta (ff2 xx yy, ff2 xx yy) == True
  , caneta (ff2 xx yy, ff2 one yy) == True

  , etaReduce [(ff xx, gg xx)] == [(ffE, ggE)]
  ]

dvl :: Typeable a => Defn -> Expr -> a
dvl  =  devl exprExpr 11

factV, sumV, andV, orV, isZeroV, isOneV, nullV, appendV :: Expr
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
             , fact' xx    =-  xx -*- (factV :$ (minus :$ xx :$ one))
             ]  where  fact' e  =  factV :$ e

fact1Defn :: Defn
fact1Defn  =  [ fact' xx  =-  if' (xx -==- zero) one (xx -*- (factV :$ (minus :$ xx :$ one)))
              ]  where  fact' e  =  factV :$ e

factCDefn :: Defn
factCDefn  =  [ fact' xx  =-  caseBool (xx -==- zero) (xx -*- (factV :$ (minus :$ xx :$ one))) one
              ]  where  fact' e  =  factV :$ e

nullDefn :: Defn
nullDefn  =  [ null' nil           =-  true
             , null' (xx -:- xxs)  =-  false
             ]  where  null' e  =  nullV :$ e

isZeroDefn :: Defn
isZeroDefn  =  [ isZero' zero  =-  true
               , isZero' xx    =-  false
               ]  where  isZero' e  =  isZeroV :$ e

isZeroCDefn :: Defn
isZeroCDefn  =  [ isZero' xx  =-  caseOrdering (xx `compare'` zero) false true false ]
  where  isZero' e  =  isZeroV :$ e

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

idDefn :: Defn
idDefn  =  [ id' xx  =-  xx ]

constDefn :: Defn
constDefn  =  [ const' xx yy  =-  xx ]

appDefn :: Defn
appDefn  =  [ ffE -$- xx  =-  ff xx ]

composeDefn :: Defn
composeDefn  =  [ (ffE -.- ggE) :$ xx  =-  ff (gg xx) ]

constRedundantDefn :: Defn
constRedundantDefn  =  [ const' zero xx  =-  zero
                       , const' xx   yy  =-  xx
                       ]

constRedundantRedundantDefn :: Defn
constRedundantRedundantDefn  =  [ const' zero zero  =-  zero
                                , const' zero xx    =-  zero
                                , const' xx   zero  =-  xx
                                , const' xx   yy    =-  xx
                                ]

constBoolRedundantDefn :: Defn
constBoolRedundantDefn  =  [ const' false false  =-  false
                           , const' false true   =-  false
                           , const' true false   =-  true
                           , const' true true    =-  true
                           ]

idListRedundantDefn :: Defn
idListRedundantDefn  =  [ id' nil           =-  nil
                        , id' (xx -:- xxs)  =-  xx -:- xxs
                        ]

constNilRedundantDefn :: Defn
constNilRedundantDefn  =  [ id' nil           =-  nil
                          , id' (xx -:- xxs)  =-  nil
                          ]

constFalseRedundantDefn :: Defn
constFalseRedundantDefn  =  [ id' nil           =-  false
                            , id' (xx -:- xxs)  =-  false
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

(=-) :: a -> b -> (a, b)
(=-) = (,)
infixr 0 =-
