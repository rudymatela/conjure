-- |
-- Module      : Conjure.Utils
-- Copyright   : (c) 2021 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- An internal module of 'Conjure'.
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
  , iterateUntil
  , mzip
  , groupOn
#if __GLASGOW_HASKELL__ < 710
  , sortOn
#endif
  , takeUntil
  , takeNextWhile
  , takeNextUntil
  , deconstructions
  )
where

import Data.List
import Data.Function
import Data.Maybe
import Data.Monoid
import Data.Tuple
import Data.Typeable

count :: (a -> Bool) -> [a] -> Int
count p  =  length . filter p

nubOn :: Eq b => (a -> b) -> [a] -> [a]
nubOn f  =  nubBy ((==) `on` f)

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

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p  =  takeWhile (not . p)

takeNextWhile :: (a -> a -> Bool) -> [a] -> [a]
takeNextWhile (?)  =  t
  where
  t (x:y:xs) | x ? y  =  x : t (y:xs)
             | otherwise  =  [x]
  t xs  =  xs

takeNextUntil :: (a -> a -> Bool) -> [a] -> [a]
takeNextUntil (?)  =  takeNextWhile (not .: (?))
  where
  (.:)  =  (.) . (.)

deconstructions :: Eq a => (a -> Bool) -> (a -> a) -> a -> [a]
deconstructions z d x  =  takeNextUntil (==)
                       .  takeUntil z
                       $  iterate d x
