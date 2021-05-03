-- tapps.hs: conjure with (portable) type applications
--
-- Copyright (C) 2021 Rudy Matela
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
  conjure "third"   third    primitives
  conjure "product" product' primitives
  conjure "product" product' primitivesWithFold

primitives :: [Expr]
primitives =
  [ val (0 :: Int)
  , val (1 :: Int)
#if __GLASGOW_HASKELL__ < 800
  , value "+" ((+) :: Int -> Int -> Int)
  , value "*" ((*) :: Int -> Int -> Int)
  , value "null" (null :: [Int] -> Bool)
  , value "head" (head :: [Int] -> Int)
  , value "tail" (tail :: [Int] -> [Int])
#else
  , value "+" ((+) @Int)
  , value "*" ((*) @Int)
-- the following #if was added just for dramatic effect (see notes below)
#if __GLASGOW_HASKELL__ < 710
  , value "null" (null @Int)
#else
  , value "null" (null @[] @Int)
#endif
  , value "head" (head @Int)
  , value "tail" (tail @Int)
#endif
  ]

primitivesWithFold :: [Expr]
#if __GLASGOW_HASKELL__ < 800
primitivesWithFold  =
    value "foldr" (foldr :: (Int -> Int -> Int) -> Int -> [Int] -> Int)
  : primitives
#else
-- the following #if was added just for dramatic effect (see notes below)
#if __GLASGOW_HASKELL__ < 710
primitivesWithFold  =  value "foldr" (foldr @Int @Int) : primitives
#else
primitivesWithFold  =  value "foldr" (foldr @[] @Int @Int) : primitives
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
