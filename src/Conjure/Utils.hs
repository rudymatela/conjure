-- |
-- Module      : Conjure.Utils
-- Copyright   : (c) 2021 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- An internal module of "Conjure".
-- This exports 'Data.List', 'Data.Maybe', 'Data.Function'
-- and a few other simple utitilites.
{-# LANGUAGE CPP #-}
module Conjure.Utils
  ( module Data.List
  , module Data.Function
  , module Data.Maybe
  , module Data.Monoid
  , module Data.Tuple
  , module Data.Typeable

  , count
  , nubOn
  , nubSort
  , iterateUntil
  , mzip
  , groupOn
#if __GLASGOW_HASKELL__ < 710
  , sortOn
#endif
  , idIO
  , mapHead
  , sets
  , headOr
  , allEqual
  , choices
  , choicesThat
  , filterAnd
  )
where

import Data.List
import Data.Function
import Data.Maybe
import Data.Monoid
import Data.Tuple
import Data.Typeable

import System.IO.Unsafe

allEqual :: Eq a => [a] -> Bool
allEqual []  =  False
allEqual [x]  =  False
allEqual [x,y]  =  x == y
allEqual (x:y:xs)  =  x == y && allEqual (y:xs)

count :: (a -> Bool) -> [a] -> Int
count p  =  length . filter p

nubOn :: Eq b => (a -> b) -> [a] -> [a]
nubOn f  =  nubBy ((==) `on` f)

nubSort :: Ord a => [a] -> [a]
nubSort  =  nnub . sort
  where
  -- linear nub of adjacent values
  nnub [] = []
  nnub [x] = [x]
  nnub (x:xs) = x : nnub (dropWhile (==x) xs)

iterateUntil :: (a -> a -> Bool) -> (a -> a) -> a -> a
iterateUntil (?) f  =  iu
  where
  iu x | x ? fx     =  x
       | otherwise  =  iu fx
    where
    fx  =  f x

mzip :: Monoid a => [a] -> [a] -> [a]
mzip [] []  =  []
mzip [] ys  =  ys
mzip xs []  =  xs
mzip (x:xs) (y:ys)  =  x <> y : mzip xs ys

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy ((==) `on` f)

#if __GLASGOW_HASKELL__ < 710
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = sortBy (compare `on` f)
#endif

-- | __WARNING:__
--   uses 'unsafePerformIO' and should only be used for debugging!
--
-- > > idIO print 10
-- > 10
-- > 10
idIO :: (a -> IO ()) -> a -> a
idIO action x  =  unsafePerformIO $ do
  action x
  return x

mapHead :: (a -> a) -> [a] -> [a]
mapHead f (x:xs)  =  f x : xs

sets :: [a] -> [[a]]
sets []  =  [[]]
sets (x:xs)  =  map (x:) ss ++ ss
  where
  ss  =  sets xs

headOr :: a -> [a] -> a
headOr x []  =  x
headOr _ (x:xs)  =  x

choices :: [a] -> [(a,[a])]
choices []  =  []
choices (x:xs)  =  (x,xs) : map (mapSnd (x:)) (choices xs)
  where
  mapSnd f (x,y)  =  (x,f y)

choicesThat :: (a -> [a] -> Bool) -> [a] -> [(a,[a])]
choicesThat (?)  =  filter (uncurry (?)) . choices

filterAnd :: (a -> Bool) -> [a] -> ([a],Bool)
filterAnd p xs  =  (xs', and ps)
  where
  xps  =  [(x,p x) | x <- xs]
  xs'  =  [x | (x,True) <- xps]
  ps   =  [p | (_,p) <- xps]
