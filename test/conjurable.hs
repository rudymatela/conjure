-- Copyright (C) 2021 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).

import Test

main :: IO ()
main  =  mainTest tests 5040

tests :: Int -> [Bool]
tests n  =
  [ True

  , holds n $ \x y -> (x <==> y)  ==  (x == (y :: ()))
  , holds n $ \x y -> (x <==> y)  ==  (x == (y :: Int))
  , holds n $ \p q -> (p <==> q)  ==  (p == (q :: Bool))

  , holds n $ \xs ys -> (xs <==> ys)  ==  (xs == (ys :: [Int]))
  , holds n $ \s1 s2 -> (s1 <==> s2)  ==  (s1 == (s2 :: String))

  , holds n $ \xy zw -> (xy <==> zw)  ==  (xy == (zw :: (Int,Int)))
  , holds n $ \xy zw -> (xy <==> zw)  ==  (xy == (zw :: (Bool,Integer)))
  , holds n $ \xy zw -> (xy <==> zw)  ==  (xy == (zw :: (String,[Int])))
  ]

-- Equality but obtained through conjurable
(<==>) :: Conjurable a => a -> a -> Bool
x <==> y  =  x == y
  where
  (==)  =  eval err . fromMaybe err $ conjureEquality x
  err  =  error "<==>: could not conjure"
infix 4 <==>
