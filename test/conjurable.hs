-- Copyright (C) 2021-2025 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).

{-# Language DeriveDataTypeable, StandaloneDeriving #-}  -- for GHC < 7.10

import Test
import Data.Dynamic

-- An Unit type that is not an Eq instance
data Unit  =  Unit  deriving Show

deriving instance Typeable Unit  -- for GHC < 7.10

instance Listable Unit where list = [Unit]
instance Name Unit where name _ = "u"
instance Conjurable Unit where
  conjureExpress = reifyExpress
  conjureTiers = reifyTiers

instance Express Unit where  expr  =  val

main :: IO ()
main  =  mainTest tests 5040

tests :: Int -> [Bool]
tests n  =
  [ True

  , conjureApplication "not"   not  ==  not' pp
  , conjureApplication "not q" not  ==  not' qq
  , conjureApplication "negate"   (negate :: Int -> Int)  ==  negate' xx
  , conjureApplication "negate i" (negate :: Int -> Int)  ==  negate' ii
  , conjureApplication "+"     ((+) :: Int -> Int -> Int)  ==  xx -+- yy
  , conjureApplication "+ i j" ((+) :: Int -> Int -> Int)  ==  ii -+- jj

  , holds n $ \x y -> (x <==> y)  ==  (x == (y :: ()))
  , holds n $ \x y -> (x <==> y)  ==  (x == (y :: Int))
  , holds n $ \p q -> (p <==> q)  ==  (p == (q :: Bool))
  , conjureEquality Unit == Nothing

  , holds n $ \xs ys -> (xs <==> ys)  ==  (xs == (ys :: [Int]))
  , holds n $ \s1 s2 -> (s1 <==> s2)  ==  (s1 == (s2 :: String))
  , conjureEquality [Unit] == Nothing

  , holds n $ \xy zw -> (xy <==> zw)  ==  (xy == (zw :: (Int,Int)))
  , holds n $ \xy zw -> (xy <==> zw)  ==  (xy == (zw :: (Bool,Integer)))
  , holds n $ \xy zw -> (xy <==> zw)  ==  (xy == (zw :: (String,[Int])))
  , conjureEquality (undefined :: (Int,Unit)) == Nothing
  , conjureEquality (undefined :: (Unit,Int)) == Nothing

  , holds n $ \xyz wvu -> (xyz <==> wvu)  ==  (xyz == (wvu :: (Int,Int,Int)))
  , holds n $ \xyz wvu -> (xyz <==> wvu)  ==  (xyz == (wvu :: (Bool,Bool,Bool)))
  , holds n $ \xyz wvu -> (xyz <==> wvu)  ==  (xyz == (wvu :: (Int,Bool,())))
  , holds n $ \xyz wvu -> (xyz <==> wvu)  ==  (xyz == (wvu :: (Bool,String,[Integer])))
  , isNothing $ conjureEquality (undefined :: (Unit,Bool,Bool))
  , isNothing $ conjureEquality (undefined :: (Bool,Unit,Bool))
  , isNothing $ conjureEquality (undefined :: (Bool,Bool,Unit))

  , holds n $ \mx my -> (mx <==> my)  ==  (mx == (my :: Maybe ()))
  , holds n $ \mx my -> (mx <==> my)  ==  (mx == (my :: Maybe Bool))
  , holds n $ \mx my -> (mx <==> my)  ==  (mx == (my :: Maybe Int))
  , holds n $ \mx my -> (mx <==> my)  ==  (mx == (my :: Maybe [Maybe Int]))
  , isNothing $ conjureEquality (undefined :: Maybe Unit)

  , holds n $ \ex ey -> (ex <==> ey)  ==  (ex == (ey :: Either () ()))
  , holds n $ \ex ey -> (ex <==> ey)  ==  (ex == (ey :: Either Bool Bool))
  , holds n $ \ex ey -> (ex <==> ey)  ==  (ex == (ey :: Either Int Int))
  , holds n $ \ex ey -> (ex <==> ey)  ==  (ex == (ey :: Either Int Bool))
  , holds n $ \ex ey -> (ex <==> ey)  ==  (ex == (ey :: Either Bool Int))
  , holds n $ \ex ey -> (ex <==> ey)  ==  (ex == (ey :: Either [Int] String))
  , isNothing $ conjureEquality (undefined :: Either Unit Bool)
  , isNothing $ conjureEquality (undefined :: Either Bool Unit)
  , isNothing $ conjureEquality (undefined :: Either Unit Unit)

  , holds n $ \t1 t2 -> (t1 <==> t2)  ==  (t1 == (t2 :: (Int,Int,Int,Int)))
  , holds n $ \t1 t2 -> (t1 <==> t2)  ==  (t1 == (t2 :: (Bool,Bool,Bool,Bool)))
  , holds n $ \t1 t2 -> (t1 <==> t2)  ==  (t1 == (t2 :: (Int,Bool,(),Char)))
  , holds n $ \t1 t2 -> (t1 <==> t2)  ==  (t1 == (t2 :: (Bool,String,[Integer],Bool)))
  , isNothing $ conjureEquality (undefined :: (Unit,Bool,Bool,Bool))
  , isNothing $ conjureEquality (undefined :: (Bool,Unit,Bool,Bool))
  , isNothing $ conjureEquality (undefined :: (Bool,Bool,Unit,Bool))
  , isNothing $ conjureEquality (undefined :: (Bool,Bool,Unit,Unit))

  , holds n $ \t1 t2 -> (t1 <==> t2)  ==  (t1 == (t2 :: (Int,Int,Int,Int,Int)))
  , holds n $ \t1 t2 -> (t1 <==> t2)  ==  (t1 == (t2 :: (Bool,Bool,Bool,Bool,Bool)))
  , holds n $ \t1 t2 -> (t1 <==> t2)  ==  (t1 == (t2 :: (Int,Bool,(),Char,String)))
  , holds n $ \t1 t2 -> (t1 <==> t2)  ==  (t1 == (t2 :: (Bool,String,[Integer],Bool,String)))
  , isNothing $ conjureEquality (undefined :: (Unit,Bool,Bool,Bool,Bool))
  , isNothing $ conjureEquality (undefined :: (Bool,Unit,Bool,Bool,Bool))
  , isNothing $ conjureEquality (undefined :: (Bool,Bool,Unit,Bool,Bool))
  , isNothing $ conjureEquality (undefined :: (Bool,Bool,Bool,Unit,Bool))
  , isNothing $ conjureEquality (undefined :: (Bool,Bool,Bool,Bool,Unit))

  , holds n $ \t1 t2 -> (t1 <==> t2)  ==  (t1 == (t2 :: (Int,Int,Int,Int,Int,Int)))
  , holds n $ \t1 t2 -> (t1 <==> t2)  ==  (t1 == (t2 :: (Bool,Bool,Bool,Bool,Bool,Bool)))
  , holds n $ \t1 t2 -> (t1 <==> t2)  ==  (t1 == (t2 :: (Int,Bool,(),Char,String,Bool)))
  , holds n $ \t1 t2 -> (t1 <==> t2)  ==  (t1 == (t2 :: (Bool,String,[Integer],Bool,String,Int)))
  , isNothing $ conjureEquality (undefined :: (Unit,Bool,Bool,Bool,Bool,Bool))
  , isNothing $ conjureEquality (undefined :: (Bool,Unit,Bool,Bool,Bool,Bool))
  , isNothing $ conjureEquality (undefined :: (Bool,Bool,Unit,Bool,Bool,Bool))
  , isNothing $ conjureEquality (undefined :: (Bool,Bool,Bool,Unit,Bool,Bool))
  , isNothing $ conjureEquality (undefined :: (Bool,Bool,Bool,Bool,Unit,Bool))
  , isNothing $ conjureEquality (undefined :: (Bool,Bool,Bool,Bool,Bool,Unit))

  , holds n $ \t1 t2 -> (t1 <==> t2)  ==  (t1 == (t2 :: (Int,Int,Int,Int,Int,Int,Int)))
  , holds n $ \t1 t2 -> (t1 <==> t2)  ==  (t1 == (t2 :: (Bool,Bool,Bool,Bool,Bool,Bool,Bool)))
  , holds n $ \t1 t2 -> (t1 <==> t2)  ==  (t1 == (t2 :: (Int,Bool,(),Char,String,Bool,Int)))
  , holds n $ \t1 t2 -> (t1 <==> t2)  ==  (t1 == (t2 :: (Bool,String,[Integer],Bool,String,Int,[Integer])))
  , isNothing $ conjureEquality (undefined :: (Unit,Bool,Bool,Bool,Bool,Bool,Bool))
  , isNothing $ conjureEquality (undefined :: (Bool,Unit,Bool,Bool,Bool,Bool,Bool))
  , isNothing $ conjureEquality (undefined :: (Bool,Bool,Unit,Bool,Bool,Bool,Bool))
  , isNothing $ conjureEquality (undefined :: (Bool,Bool,Bool,Unit,Bool,Bool,Bool))
  , isNothing $ conjureEquality (undefined :: (Bool,Bool,Bool,Bool,Unit,Bool,Bool))
  , isNothing $ conjureEquality (undefined :: (Bool,Bool,Bool,Bool,Bool,Unit,Bool))
  , isNothing $ conjureEquality (undefined :: (Bool,Bool,Bool,Bool,Bool,Bool,Unit))

  , conjureArgumentHoles (undefined :: Bool -> Int -> Bool -> ()) == [b_, i_, b_]

  , length (conjureCases (undefined :: ()))      == 1
  , length (conjureCases (undefined :: Bool))    == 2
  , length (conjureCases (undefined :: Int))     == 0
  , length (conjureCases (undefined :: Integer)) == 0
  , length (conjureCases (undefined :: [Int]))   == 2
  , length (conjureCases (undefined :: [Bool]))  == 2

  , conjureCases (undefined :: Bool) == [false, true]
  , conjureCases (undefined :: Int) == []
  , conjureCases (undefined :: [Int]) == [nil, i_ -:- is_]
  , conjureCases (undefined :: [Bool]) == [nilBool, b_ -:- bs_]
  , conjureCases (undefined :: Maybe Int) == [nothing, just i_]

  , map length (conjureArgumentCases (undefined :: () -> Bool -> Int -> [Int] -> [Bool] -> ()))
    == [1, 2, 0, 2, 2]

  , conjurePats 1 [zero, one] "f" (undefined :: Int -> Int)
    == [ [ [ ff xx
           ]
         ]
       , [ [ ff zero
           , ff xx
           ]
         ]
       , [ [ ff one
           , ff xx
           ]
         ]
       , [ [ ff zero
           , ff one
           , ff xx
           ]
         ]
       ]

  , conjurePats 1 [] "f" (undefined :: [Int] -> Int)
    == [ [ [ ffs xxs
           ]
         ]
       , [ [ ffs nilInt
           , ffs (xx -:- xxs)
           ]
         ]
       ]

  , take 3 (conjurePats 1 [zero, one] "?" (undefined :: Int -> Int -> Int))
    == [ [ [ xx -?- yy
           ]
         ]
       , [ [ xx -?- zero
           , xx -?- yy
           ]
         , [ zero -?- xx
           , xx -?- yy
           ]
         ]
       , [ [ xx -?- one
           , xx -?- yy
           ]
         , [ zero -?- xx
           , xx -?- zero
           , xx -?- yy
           ]
         , [ zero -?- zero
           , zero -?- xx
           , xx -?- yy
           ]
         , [ one -?- xx
           , xx -?- yy
           ]
         ]
       ]

  , mapT length (conjurePats 1 [zero, one] "foo" (undefined :: [Int] -> Int -> Int))
    == [[1],[2,2],[2,3,3],[3,3,4,3],[4,4,4,4],[5,4,5],[5,5],[6]]


  , mapT length (conjurePats 1 [zero, one] "foo" (undefined :: Int -> [Int] -> Int))
    == [[1],[2,2],[3,3,2],[4,3,3,3],[4,4,4,4],[5,5,5],[6]]

  , conjurePats 1 [] "foo" (undefined :: [Int] -> [Char] -> Int)
    == [ [ [ ffoo xxs ccs
           ]
         ]
       , [ [ ffoo xxs emptyString
           , ffoo xxs (cc -:- ccs)
           ]
         , [ ffoo nilInt       ccs
           , ffoo (xx -:- xxs) ccs
           ]
         ]
       , [ [ ffoo nilInt ccs
           , ffoo (xx -:- xxs) emptyString
           , ffoo (xx -:- xxs) (cc -:- ccs)
           ]
         , [ ffoo nilInt emptyString
           , ffoo nilInt (cc -:- ccs)
           , ffoo (xx -:- xxs) ccs
           ]
         ]
       , [ [ ffoo nilInt emptyString
           , ffoo nilInt (cc -:- ccs)
           , ffoo (xx -:- xxs) emptyString
           , ffoo (xx -:- xxs) (cc -:- ccs)
           ]
         ]
       ]

  , fromDynamic (conjureDynamicEq ((+) :: Int -> Int -> Int) `dynApp` toDyn (1::Int) `dynApp` toDyn (2::Int))
    == Just False
  , fromDynamic (conjureDynamicEq ((+) :: Int -> Int -> Int) `dynApp` toDyn (1::Int) `dynApp` toDyn (1::Int))
    == Just True

  , isNumeric one
  , isNumeric (val (1 :: Integer))
  , isNumeric bee == False
  , isNumeric (expr [0::Int]) == False
  , isNumeric true == False

  -- the following conjurableOK calls take 5 seconds to compile!
  , conjurableOK (undefined :: ())
  , conjurableOK (undefined :: Bool)
  , conjurableOK (undefined :: Int)
  , conjurableOK (undefined :: Char)
  , conjurableOK (undefined :: Integer)
  , conjurableOK (undefined :: Ordering)
  , conjurableOK (undefined :: Float)
  , conjurableOK (undefined :: Double)
  , conjurableOK (undefined :: Rational)

  , conjurableOK (undefined :: A)
  , conjurableOK (undefined :: B)
  , conjurableOK (undefined :: C)
  , conjurableOK (undefined :: D)
  , conjurableOK (undefined :: E)
  , conjurableOK (undefined :: F)

  , conjurableOK (undefined :: [()])
  , conjurableOK (undefined :: [Bool])
  , conjurableOK (undefined :: [Int])
  , conjurableOK (undefined :: String)

  , conjurableOK (undefined :: ((),()))
  , conjurableOK (undefined :: (Bool,Bool))
  , conjurableOK (undefined :: (Int,Int))
  , conjurableOK (undefined :: (Char,String))
  , conjurableOK (undefined :: (Bool,[Int]))

  , conjurableOK (undefined :: (Int,Int,Int))
  , conjurableOK (undefined :: (Bool,Int,Char))
  , conjurableOK (undefined :: ([Int],[Bool],String))

  , conjurableOK (undefined :: Maybe ())
  , conjurableOK (undefined :: Maybe Bool)
  , conjurableOK (undefined :: Maybe Int)
  , conjurableOK (undefined :: Maybe Char)
  , conjurableOK (undefined :: Maybe String)

  , conjurableOK (undefined :: Either () ())
  , conjurableOK (undefined :: Either Bool Bool)
  , conjurableOK (undefined :: Either Int Int)
  , conjurableOK (undefined :: Either String Int)
  , conjurableOK (undefined :: Either String Char)

  , conjurableOK (undefined :: (Int,Int,Int,Int))
  , conjurableOK (undefined :: (Int,Int,Int,Int,Int))
  , conjurableOK (undefined :: (Int,Int,Int,Int,Int,Int))
  , conjurableOK (undefined :: (Int,Int,Int,Int,Int,Int,Int))
  , conjurableOK (undefined :: (Int,Int,Int,Int,Int,Int,Int,Int))
  , conjurableOK (undefined :: (Int,Int,Int,Int,Int,Int,Int,Int,Int))
  , conjurableOK (undefined :: (Int,Int,Int,Int,Int,Int,Int,Int,Int,Int))
  , conjurableOK (undefined :: (Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int))
  , conjurableOK (undefined :: (Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int))

  -- little sanity check (conjurableOK should catch it anyway)
  , take 6 (conjureListFor (undefined :: Int) i_)
    == [zero, one, minusOne, two, minusTwo, three]

  , conjureTestDefn 6 12 "factorial n" fact
    == [ (value "factorial" fact :$ one,   one)
       , (value "factorial" fact :$ two,   two)
       , (value "factorial" fact :$ three, six)
       , (value "factorial" fact :$ four,  val (24 :: Int))
       ]

  , conjureTestDefn 4 6 "sum" (sum :: [Int] -> Int)
    == [ (sum' nil, zero)
       , (sum' (val [0 :: Int]),    zero)
       , (sum' (val [0, 0 :: Int]), zero)
       , (sum' (val [1 :: Int]),    one)
       ]

  , conjureTestDefn 3 4 ":" ((:) :: Int -> [Int] -> [Int])
    == [ (zero -:- nil,          zero -:- nil)
       , (zero -:- val [0::Int], zero -:- zero -:- nil)
       , (one -:- nil,           one -:- nil)
       ]

  , conjureArgumentPatterns 1 [] (undefined :: Int -> ())
      ==  conjureArgumentPats [] (undefined :: Int -> ())

  , conjureArgumentPatterns 1 [] (undefined :: Bool -> ())
      ==  conjureArgumentPats [] (undefined :: Bool -> ())

  , conjureArgumentPatterns 1 [] (undefined :: [Bool] -> ())
      ==  conjureArgumentPats [] (undefined :: [Bool] -> ())

  , conjureArgumentPatterns 1 [zero, one] (undefined :: [Int] -> ())
      ==  conjureArgumentPats [zero, one] (undefined :: [Int] -> ())

  , conjureArgumentPatterns 1 [] (undefined :: [Maybe Bool] -> ())
      ==  conjureArgumentPats [] (undefined :: [Maybe Bool] -> ())

  , conjureArgumentPatterns 1 [] (undefined :: Maybe Bool -> ())
      ==  conjureArgumentPats [] (undefined :: Maybe Bool -> ())

  , conjureArgumentPatterns 1 [zero, one] (undefined :: Int -> ())
      ==  conjureArgumentPats [zero, one] (undefined :: Int -> ())
  ]

isNumeric :: Expr -> Bool
isNumeric  =  conjureIsNumeric (undefined :: [Int] -> [Char] -> [Bool] -> [Integer])

ffs :: Expr -> Expr
ffs e  =  ffE :$ e
  where
  ffE  =  var "f" (undefined :: [Int] -> Int)

ffoo :: Expr -> Expr -> Expr
ffoo ex ey  =  headOr err . mapMaybe ($$ ey) . mapMaybe ($$ ex)
            $  [ var "foo" (undefined :: [Int] -> [Char] -> Int)
               , var "foo" (undefined :: Int -> [Int] -> Int)
               , var "foo" (undefined :: [Int] -> Int -> Int)
               ]
  where
  err  =  error $ "ffoo: cannot apply `foo :: * -> * -> *` to `"
               ++ show ex ++ "' and `" ++ show ey ++ "'.  Unhandled types?"


-- Equality but obtained through conjurable
(<==>) :: Conjurable a => a -> a -> Bool
x <==> y  =  x == y
  where
  (==)  =  eval err . fromMaybe err $ conjureEquality x
  err  =  error "<==>: could not conjure"
infix 4 <==>


-- to be used in the test of conjureTestBinds and related functions
fact :: Int -> Int
fact 1  =  1
fact 2  =  2
fact 3  =  6
fact 4  =  24
