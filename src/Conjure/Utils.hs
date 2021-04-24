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

  , fromLeft
  , fromRight
  , elemBy
  , listEq
  , listOrd
  , maybeEq
  , maybeOrd
  , eitherEq
  , eitherOrd
  , pairEq
  , pairOrd
  , tripleEq
  , tripleOrd
  , quadrupleEq
  , quadrupleOrd
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

mapHead :: (a -> a) -> [a] -> [a]
mapHead f (x:xs)  =  f x : xs
mapHead _ []  =  error "Conjure.Utils.mapHead: empty list"

-- note these versions of fromLeft and fromRight differ from the ones of
-- Data.Either since 4.10.0.0.
fromLeft :: Either a b -> a
fromLeft (Left x) = x
fromLeft _        = error "fromLeft: not a left"

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _         = error "fromRight: not a right"

elemBy :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy (==) x = any (== x)

listEq :: (a -> a -> Bool) -> [a] -> [a] -> Bool
listEq (==) []     []     = True
listEq (==) (x:xs) []     = False
listEq (==) []     (y:ys) = False
listEq (==) (x:xs) (y:ys) = x == y && listEq (==) xs ys

listOrd :: (a -> a -> Bool) -> [a] -> [a] -> Bool
listOrd (<=) []     []     = True
listOrd (<=) (x:xs) []     = False
listOrd (<=) []     (y:ys) = True
listOrd (<=) (x:xs) (y:ys) = x <  y
                          || x == y && listOrd (<=) xs ys
  where
  x <  y = x <= y && not (y <= x)
  x == y = x <= y &&      y <= x

maybeEq :: (a -> a -> Bool) -> Maybe a -> Maybe a -> Bool
maybeEq (==) Nothing  Nothing  = True
maybeEq (==) Nothing  (Just y) = False
maybeEq (==) (Just x) Nothing  = False
maybeEq (==) (Just x) (Just y) = x == y

maybeOrd :: (a -> a -> Bool) -> Maybe a -> Maybe a -> Bool
maybeOrd (<=) Nothing  Nothing  = True
maybeOrd (<=) Nothing  (Just y) = True
maybeOrd (<=) (Just x) Nothing  = False
maybeOrd (<=) (Just x) (Just y) = x <= y

eitherEq :: (a -> a -> Bool) -> (b -> b -> Bool) -> Either a b -> Either a b -> Bool
eitherEq (==) _ (Left  x) (Left  y) = x == y
eitherEq _ (==) (Right x) (Right y) = x == y
eitherEq _ _ _ _ = False

eitherOrd :: (a -> a -> Bool) -> (b -> b -> Bool) -> Either a b -> Either a b -> Bool
eitherOrd (<=) _ (Left  x) (Left  y) = x <= y
eitherOrd _ (<=) (Right x) (Right y) = x <= y
eitherOrd _    _ (Left  _) (Right _) = True
eitherOrd _    _ (Right _) (Left  _) = False

pairEq :: (a -> a -> Bool) -> (b -> b -> Bool) -> (a,b) -> (a,b) -> Bool
pairEq (==.) (.==) (x1,y1) (x2,y2) = x1 ==. x2 && y1 .== y2

pairOrd :: (a -> a -> Bool) -> (b -> b -> Bool) -> (a,b) -> (a,b) -> Bool
pairOrd (<=.) (.<=) (x1,y1) (x2,y2) = x1 <. x2
                                   || x1 ==. x2 && y1 .<= y2
  where
  x <.  y = x <=. y && not (y <=. x)
  x ==. y = x <=. y &&      y <=. x

tripleEq :: (a -> a -> Bool) -> (b -> b -> Bool) -> (c -> c -> Bool)
         -> (a,b,c) -> (a,b,c) -> Bool
tripleEq (==..) (.==.) (..==) (x1,y1,z1) (x2,y2,z2) =
  x1 ==.. x2 && y1 .==. y2 && z1 ..== z2

tripleOrd :: (a -> a -> Bool) -> (b -> b -> Bool) -> (c -> c -> Bool)
          -> (a,b,c) -> (a,b,c) -> Bool
tripleOrd (<=..) (.<=.) (..<=) (x1,y1,z1) (x2,y2,z2) =
  x1 <.. x2 || x1 ==.. x2 && pairOrd (.<=.) (..<=) (y1,z1) (y2,z2)
  where
  x <..  y = x <=.. y && not (y <=.. x)
  x ==.. y = x <=.. y &&      y <=.. x

quadrupleEq :: (a->a->Bool) -> (b->b->Bool) -> (c->c->Bool) -> (d->d->Bool)
            -> (a,b,c,d) -> (a,b,c,d) -> Bool
quadrupleEq (==...) (.==..) (..==.) (...==) (x1,y1,z1,w1) (x2,y2,z2,w2) =
  x1 ==... x2 && y1 .==.. y2 && z1 ..==. z2 && w1 ...== w2

quadrupleOrd :: (a->a->Bool) -> (b->b->Bool) -> (c->c->Bool) -> (d->d->Bool)
             -> (a,b,c,d) -> (a,b,c,d) -> Bool
quadrupleOrd (<=...) (.<=..) (..<=.) (...<=) (x1,y1,z1,w1) (x2,y2,z2,w2) =
  x1 <... x2 || x1 ==... x2 && tripleOrd (.<=..) (..<=.) (...<=) (y1,z1,w1) (y2,z2,w2)
  where
  x <...  y = x <=... y && not (y <=... x)
  x ==... y = x <=... y &&      y <=... x
