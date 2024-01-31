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

  , take 3 (conjurePats [zero, one] "?" (undefined :: Int -> Int -> Int))
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

  , mapT length (conjurePats [zero, one] "foo" (undefined :: [Int] -> Int -> Int))
    == [[1],[2,2],[2,3,3],[3,3,4,3],[4,4,4,4],[5,4,5],[5,5],[6]]


  , mapT length (conjurePats [zero, one] "foo" (undefined :: Int -> [Int] -> Int))
    == [[1],[2,2],[3,3,2],[4,3,3,3],[4,4,4,4],[5,5,5],[6]]

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
  , isDecon (const' zero i_) == True -- TODO: disallow?
  , isDecon (const' nil is_) == True -- TODO: disallow?
  , isDecon (const' one i_)  == False -- does not deconstruct 1

  -- negative "deconstructions"
  , isDecon (minus :$ zero :$ i_) == True -- TODO: disallow?
  , isDecon (minus :$ one :$ i_)  == True -- TODO: disallow?

  -- boolean "deconstructions"
  , isDecon (not' b_) == True -- TODO: disallow?
  , isDecon (false -||- b_) == True -- TODO: disallow?

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
  ]
  where
  -- TODO: remove once these are available on Express.Fixtures
  dropE  =  value "drop" (drop :: Int -> [Int] -> [Int])
  takeE  =  value "take" (take :: Int -> [Int] -> [Int])
  drop' en exs  =  dropE :$ en :$ exs
  take' en exs  =  takeE :$ en :$ exs

isDecon :: Expr -> Bool
isDecon =  conjureIsDeconstruction (undefined :: [Int] -> [Char] -> [Bool]) 60

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
