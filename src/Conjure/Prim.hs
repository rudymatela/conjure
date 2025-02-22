-- |
-- Module      : Conjure.Prim
-- Copyright   : (c) 2021-2025 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of "Conjure".
--
-- The 'Prim' type and utilities involving it.
--
-- You are probably better off importing "Conjure".
module Conjure.Prim
  ( Prim
  , prim
  , pr
  , prif
  , primOrdCaseFor
  , guard
  , cjHoles
  , cjTiersFor
  , cjAreEqual
  , cjMkEquation
  )
where

import Conjure.Conjurable
import Conjure.Expr
import Test.LeanCheck.Error (errorToFalse)
import Test.LeanCheck.Utils


-- | A primitive expression (paired with instance reification).
--   Create 'Prim's with 'pr' and 'prim'.
type Prim  =  (Expr, Reification)


-- | Provides a primitive value to Conjure.
--   To be used on 'Show' instances.
--   (cf. 'prim')
--
-- > conjure "fun" fun [ pr False
-- >                   , pr True
-- >                   , pr (0 :: Int)
-- >                   , pr (1 :: Int)
-- >                   , ...
-- >                   ]
--
-- Argument types have to be monomorphized,
-- so use type bindings when applicable.
pr :: (Conjurable a, Show a) => a -> Prim
pr x  =  (val x, conjureType x)


-- | Provides a primitive value to Conjure.
--   To be used on values that are not 'Show' instances
--   such as functions.
--   (cf. 'pr')
--
-- > conjure "fun" fun [ ...
-- >                   , prim "&&" (&&)
-- >                   , prim "||" (||)
-- >                   , prim "+" ((+) :: Int -> Int -> Int)
-- >                   , prim "*" ((*) :: Int -> Int -> Int)
-- >                   , prim "-" ((-) :: Int -> Int -> Int)
-- >                   , ...
-- >                   ]
--
-- Argument types have to be monomorphized,
-- so use type bindings when applicable.
prim :: Conjurable a => String -> a -> Prim
prim s x  =  (value s x, conjureType x)


-- | Provides an if condition bound to the given return type.
--
-- This should be used when one wants Conjure to consider
-- if-expressions at all:
--
-- > last' :: [Int] -> Int
-- > last' [x]  =  x
-- > last' [x,y]  =  y
-- > last' [x,y,z]  =  z
--
-- > > conjure "last" last' [ pr ([] :: [Int])
-- > >                      , prim ":" ((:) :: Int -> [Int] -> [Int])
-- > >                      , prim "null" (null :: [Int] -> Bool)
-- > >                      , prif (undefined :: Int)
-- > >                      , prim "undefined" (undefined :: Int)
-- > >                      ]
-- > last :: [Int] -> Int
-- > -- 0.0s, testing 360 combinations of argument values
-- > -- 0.0s, pruning with 5/5 rules
-- > -- ...   ...   ...   ...   ...
-- > -- 0.0s, 4 candidates of size 7
-- > -- 0.0s, tested 2 candidates
-- > last []  =  undefined
-- > last (x:xs)  =  if null xs
-- >                 then x
-- >                 else last xs
prif :: Conjurable a => a -> Prim
prif x  =  (ifFor x, conjureType x)


-- | Provides an if condition bound to the conjured function's return type.
--
-- Guards are only alllowed at the root fo the RHS.
guard :: Prim
guard  =  (guardFor (undefined :: Bool), conjureType (undefined :: Bool))
-- internally we always return a guard of the Bool return type,
-- this is replaced by Conjure when enumerating candidates


-- | Provides a case condition bound to the given return type.
--
-- This should be used when one wants Conjure to consider ord-case expressions:
--
-- > > conjure "mem" mem
-- > >   [ pr False
-- > >   , pr True
-- > >   , prim "`compare`" (compare :: Int -> Int -> Ordering)
-- > >   , primOrdCaseFor (undefined :: Bool)
-- > >   ]
-- > mem :: Int -> Tree -> Bool
-- > -- ...   ...   ...   ...   ...
-- > -- 0.0s, 384 candidates of size 12
-- > -- 0.0s, tested 346 candidates
-- > mem x Leaf  =  False
-- > mem x (Node t1 y t2)  =  case x `compare` y of
-- >                          LT -> mem x t1
-- >                          EQ -> True
-- >                          GT -> mem x t2
primOrdCaseFor :: Conjurable a => a -> Prim
primOrdCaseFor x  =  (caseForOrd x, conjureType x)


-- the following functions mirror their "conjure" counterparts from
-- Conjure.Conjurable but need a list of Prims instead of a Conjurable
-- representative.

-- | Computes a list of 'Reification1's from a list of 'Prim's.
--
-- This function mirrors functionality of 'conjureReification'.
cjReification :: [Prim] -> [Reification1]
cjReification ps  =  nubOn (\(eh,_,_,_,_,_) -> eh)
                  $  foldr (.) id (map snd ps) [conjureReification1 bool]

-- | Computes a list of holes encoded as 'Expr's from a list of 'Prim's.
--
-- This function mirrors functionality from 'conjureHoles'.
cjHoles :: [Prim] -> [Expr]
cjHoles ps  =  [eh | (eh,_,Just _,_,_,_) <- cjReification ps]

-- | Computes a function that equates two 'Expr's from a list of 'Prim's.
--
-- This function mirrors functionality from 'conjureMkEquation'.
cjMkEquation :: [Prim] -> Expr -> Expr -> Expr
cjMkEquation ps  =  mkEquation [eq | (_,Just eq,_,_,_,_) <- cjReification ps]

-- | Given a list of 'Prim's,
--   computes a function that checks whether two 'Expr's are equal
--   up to a given number of tests.
cjAreEqual :: [Prim] -> Int -> Expr -> Expr -> Bool
cjAreEqual ps maxTests  =  (===)
  where
  (-==-)  =  cjMkEquation ps
  e1 === e2  =  isTrue $ e1 -==- e2
  isTrue  =  all (errorToFalse . eval False) . gs
  gs  =  take maxTests . cjGrounds ps

-- | Given a list of 'Prim's,
--   computes a grounds function that lists
--   ground expressions of an 'Expr'.
cjGrounds :: [Prim] -> Expr -> [Expr]
cjGrounds  =  grounds . cjTiersFor

-- | Given a list of 'Prim's,
--   returns a function that given an 'Expr'
--   will return tiers of test 'Expr' values.
--
-- This is used in 'cjAreEqual'.
cjTiersFor :: [Prim] -> Expr -> [[Expr]]
cjTiersFor ps e  =  tf allTiers
  where
  allTiers :: [ [[Expr]] ]
  allTiers  =  [etiers | (_,_,Just etiers,_,_,_) <- cjReification ps]
  tf []  =  [[e]] -- no tiers found, keep variable
  tf (etiers:etc)  =  case etiers of
                      ((e':_):_) | typ e' == typ e -> etiers
                      _                            -> tf etc
