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
  )
where

import Data.List
import Data.Function
import Data.Maybe
import Data.Monoid
import Data.Tuple
import Data.Typeable

import System.IO.Unsafe

-- | Checks if all elements of a list are equal.
allEqual :: Eq a => [a] -> Bool
allEqual []  =  False
allEqual [x]  =  False
allEqual [x,y]  =  x == y
allEqual (x:y:xs)  =  x == y && allEqual (y:xs)

-- | Counts the number of occurrences on a list.
count :: (a -> Bool) -> [a] -> Int
count p  =  length . filter p

-- | Nubs using a given field.
nubOn :: Eq b => (a -> b) -> [a] -> [a]
nubOn f  =  nubBy ((==) `on` f)

-- | Equivalent to @nub . sort@ but running in /O(n log n)/.
nubSort :: Ord a => [a] -> [a]
nubSort  =  nnub . sort
  where
  -- linear nub of adjacent values
  nnub [] = []
  nnub [x] = [x]
  nnub (x:xs) = x : nnub (dropWhile (==x) xs)

-- | Zips 'Monoid' values leaving trailing values.
--
-- > > mzip ["ab","cd"] ["ef"]
-- > ["abef","cd"]
mzip :: Monoid a => [a] -> [a] -> [a]
mzip [] []  =  []
mzip [] ys  =  ys
mzip xs []  =  xs
mzip (x:xs) (y:ys)  =  x <> y : mzip xs ys

-- | Group values using a given field selector.
groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy ((==) `on` f)

#if __GLASGOW_HASKELL__ < 710
-- | 'sort' on a given field.
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

-- | Applies a function to the head of a list.
mapHead :: (a -> a) -> [a] -> [a]
mapHead f (x:xs)  =  f x : xs

-- | Return sets of values based on the list.
--
-- The values in the list must me unique.
sets :: [a] -> [[a]]
sets []  =  [[]]
sets (x:xs)  =  map (x:) ss ++ ss
  where
  ss  =  sets xs

-- | Like 'head' but allows providing a default value.
headOr :: a -> [a] -> a
headOr x []  =  x
headOr _ (x:xs)  =  x

-- | Lists choices of values.
choices :: [a] -> [(a,[a])]
choices []  =  []
choices (x:xs)  =  (x,xs) : map (mapSnd (x:)) (choices xs)
  where
  mapSnd f (x,y)  =  (x,f y)

-- | Lists choices of values that follow a property.
choicesThat :: (a -> [a] -> Bool) -> [a] -> [(a,[a])]
choicesThat (?)  =  filter (uncurry (?)) . choices
