-- Copyright (C) 2021 Rudy Matela
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

  , length (conjureCases (undefined :: ()))      == 1
  , length (conjureCases (undefined :: Bool))    == 2
  , length (conjureCases (undefined :: Int))     == 0
  , length (conjureCases (undefined :: Integer)) == 0
  , length (conjureCases (undefined :: [Int]))   == 2
  , length (conjureCases (undefined :: [Bool]))  == 2

  , conjurePats [zero, one] "f" (undefined :: Int -> Int)
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

  , conjurePats [] "f" (undefined :: [Int] -> Int)
    == [ [ [ ffs xxs
           ]
         ]
       , [ [ ffs nilInt
           , ffs (xx -:- xxs)
           ]
         ]
       ]

  , take 4 (conjurePats [zero, one] "?" (undefined :: Int -> Int -> Int))
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
         , [ zero -?- zero
           , zero -?- xx
           , xx -?- zero
           , xx -?- yy
           ]
         , [ one -?- xx
           , xx -?- yy
           ]
         ]
       , [ [ xx -?- zero
           , xx -?- one
           , xx -?- yy
           ]
         , [ zero -?- one
           , zero -?- xx
           , xx -?- one
           , xx -?- yy
           ]
         , [ one -?- zero
           , one -?- xx
           , xx -?- zero
           , xx -?- yy
           ]
         , [ zero -?- xx
           , one -?- xx
           , xx -?- yy
           ]
         ]
       ]

  , concat (conjurePats [zero, one] "foo" (undefined :: [Int] -> Int -> Int))
    == [ [ ffoo xxs xx
         ]
       , [ ffoo xxs zero
         , ffoo xxs xx
         ]
       , [ ffoo nil xx
         , ffoo (xx -:- xxs) yy
         ]
       , [ ffoo xxs one
         , ffoo xxs xx
         ]
       , [ ffoo nil zero
         , ffoo nil xx
         , ffoo (xx -:- xxs) zero
         , ffoo (xx -:- xxs) yy
         ]
       , [ ffoo xxs zero
         , ffoo xxs one
         , ffoo xxs xx
         ]
       , [ ffoo nil one
         , ffoo nil xx
         , ffoo (xx -:- xxs) one
         , ffoo (xx -:- xxs) yy
         ]
       , [ ffoo nil zero
         , ffoo nil one
         , ffoo nil xx
         , ffoo (xx -:- xxs) zero
         , ffoo (xx -:- xxs) one
         , ffoo (xx -:- xxs) yy
         ]
       ]

  , concat (conjurePats [zero, one] "foo" (undefined :: Int -> [Int] -> Int))
    == [ [ ffoo xx xxs
         ]
       , [ ffoo xx nil
         , ffoo xx (yy -:- xxs)
         ]
       , [ ffoo zero xxs
         , ffoo xx xxs
         ]
       , [ ffoo zero nil
         , ffoo zero (xx -:- xxs)
         , ffoo xx nil
         , ffoo xx (yy -:- xxs)
         ]
       , [ ffoo one xxs
         , ffoo xx xxs
         ]
       , [ ffoo one nil
         , ffoo one (xx -:- xxs)
         , ffoo xx nil
         , ffoo xx (yy -:- xxs)
         ]
       , [ ffoo zero xxs
         , ffoo one xxs
         , ffoo xx xxs
         ]
       , [ ffoo zero nil
         , ffoo zero (xx -:- xxs)
         , ffoo one nil
         , ffoo one (xx -:- xxs)
         , ffoo xx nil
         , ffoo xx (yy -:- xxs)
         ]
       ]

  , conjurePats [] "foo" (undefined :: [Int] -> [Char] -> Int)
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
       , [ [ ffoo nilInt emptyString
           , ffoo nilInt (cc -:- ccs)
           , ffoo (xx -:- xxs) emptyString
           , ffoo (xx -:- xxs) (cc -:- ccs)
           ]
         ]
       ]

  , conjureIsDeconstructor (undefined :: Int -> Int) 60 (value "dec" (subtract 1 :: Int -> Int))
    == True

  , conjureIsDeconstructor (undefined :: Int -> Int) 60 (value "halve" ((`div` 2) :: Int -> Int))
    == True

  , conjureIsDeconstructor (undefined :: Int -> Int) 60 (value "double" ((*2) :: Int -> Int))
    == False

  , conjureIsDeconstructor (undefined :: Int -> Int) 60 (value "inc" ((+1) :: Int -> Int))
    == False -- _almost_ converges half the time

  , conjureIsDeconstructor (undefined :: [Int] -> Int) 60 (value "tail" (tail :: [Int] -> [Int]))
    == True

  , conjureIsDeconstructor (undefined :: [Int] -> Int) 60 (value "prep" ((0:) :: [Int] -> [Int]))
    == False

  , isDecon (minus :$ i_ :$ one) == True
  , isDecon (minusOne -+- i_)    == True
  , isDecon (div' i_ two)        == True
  , isDecon (tail' is_)          == True
  , isDecon (init' is_)          == True
  , isDecon (mod' i_ two)        == True
  , isDecon (mod' i_ xx)         == True

  , isDecon (div' xx yy)         == False -- must have a hole to indicate the value being deconstructed
  , isDecon (div' i_ i_)         == False -- two holes are not allowed
  , isDecon (head' is_)          == False -- must deconstruct to the same type
  , isDecon (i_ -*- two)         == False -- increases the size

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

  , fromDynamic (conjureDynamicEq ((+) :: Int -> Int -> Int) `dynApp` toDyn (1::Int) `dynApp` toDyn (2::Int))
    == Just False
  , fromDynamic (conjureDynamicEq ((+) :: Int -> Int -> Int) `dynApp` toDyn (1::Int) `dynApp` toDyn (1::Int))
    == Just True
  ]

isDecon :: Expr -> Bool
isDecon =  conjureIsDeconstruction (undefined :: [Int] -> [Char] -> [Bool]) 60

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
