-- Copyright (C) 2021 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).

{-# Language DeriveDataTypeable, StandaloneDeriving #-}  -- for GHC < 7.10

import Test

-- An Unit type that is not an Eq instance
data Unit  =  Unit  deriving Show

deriving instance Typeable Unit  -- for GHC < 7.10

instance Listable Unit where list = [Unit]
instance Conjurable Unit where
  conjureTiers = reifyTiers

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

  , conjurePats "foo" (undefined :: [Int] -> [Char] -> Int)
    == [ [ [ ffoo is_ cs_
           ]
         ]
       , [ [ ffoo is_ emptyString
           , ffoo is_ (c_ -:- cs_)
           ]
         , [ ffoo nilInt       cs_
           , ffoo (i_ -:- is_) cs_
           ]
         ]
       , [ [ ffoo nilInt emptyString
           , ffoo nilInt (c_ -:- cs_)
           , ffoo (i_ -:- is_) emptyString
           , ffoo (i_ -:- is_) (c_ -:- cs_)
           ]
         ]
       ]
  ]

cs_ :: Expr
cs_  =  hole (undefined :: String)

ffoo :: Expr -> Expr -> Expr
ffoo e1 e2  =  fooE :$ e1 :$ e2
  where
  fooE  =  var "foo" (undefined :: [Int] -> [Char] -> Int)

-- Equality but obtained through conjurable
(<==>) :: Conjurable a => a -> a -> Bool
x <==> y  =  x == y
  where
  (==)  =  eval err . fromMaybe err $ conjureEquality x
  err  =  error "<==>: could not conjure"
infix 4 <==>
