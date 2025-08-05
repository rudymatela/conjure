-- tapps.hs: conjure with (portable) type applications
--
-- Copyright (C) 2021-2025 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ < 800
#else
{-# LANGUAGE TypeApplications #-}
#endif

import Conjure

third :: [Int] -> Int
third [x,y,z]  =  z
third [x,y,z,w]  =  z

product' :: [Int] -> Int
product' [x]      =  x
product' [x,y]    =  x*y
product' [x,y,z]  =  x*y*z

main :: IO ()
main = do
  conjure "third"   third    ingredients
  conjure "product" product' ingredients
  conjure "product" product' ingredientsWithFold

ingredients :: [Ingredient]
ingredients =
  [ con (0 :: Int)
  , con (1 :: Int)
#if __GLASGOW_HASKELL__ < 800
  , fun "+" ((+) :: Int -> Int -> Int)
  , fun "*" ((*) :: Int -> Int -> Int)
  , fun "null" (null :: [Int] -> Bool)
  , fun "head" (head :: [Int] -> Int)
  , fun "tail" (tail :: [Int] -> [Int])
#else
  , fun "+" ((+) @Int)
  , fun "*" ((*) @Int)
-- the following #if was added just for dramatic effect (see notes below)
#if __GLASGOW_HASKELL__ < 710
  , fun "null" (null @Int)
#else
  , fun "null" (null @[] @Int)
#endif
  , fun "head" (head @Int)
  , fun "tail" (tail @Int)
#endif
  ]

ingredientsWithFold :: [Ingredient]
#if __GLASGOW_HASKELL__ < 800
ingredientsWithFold  =
    fun "foldr" (foldr :: (Int -> Int -> Int) -> Int -> [Int] -> Int)
  : ingredients
#else
-- the following #if was added just for dramatic effect (see notes below)
#if __GLASGOW_HASKELL__ < 710
ingredientsWithFold  =  fun "foldr" (foldr @Int @Int) : ingredients
#else
ingredientsWithFold  =  fun "foldr" (foldr @[] @Int @Int) : ingredients
#endif
#endif

-- Some notes:
--
-- From GHC 7.8 to GHC 7.10,
-- null, foldr and other functions were generalized
-- from lists to foldable containers.
-- The type application for the old version of null
-- is different than the one for the new version.
--
-- In the case of this file this is not really an issue
-- as the above change was introduced before TypeApplications even existed.
--
-- This illustrates that
-- we may have to use a different type application patterns
-- when using different versions of libraries.
-- In some cases, code using TypeApplications may be
-- less portable than it's full type binding counterparts.
