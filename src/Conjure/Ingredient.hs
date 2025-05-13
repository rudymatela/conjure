-- |
-- Module      : Conjure.Ingredient
-- Copyright   : (c) 2021-2025 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of "Conjure".
--
-- The 'Ingredient' type and utilities involving it.
--
-- You are probably better off importing "Conjure".
module Conjure.Ingredient
  ( Ingredient
  , con
  , fun
  , iif
  , ordcase
  , guard
  , cjHoles
  , cjTiersFor
  , cjAreEqual
  , cjMkEquation
  , Prim
  , pr
  , prim
  , prif
  , primOrdCaseFor
  )
where

import Conjure.Conjurable
import Conjure.Expr
import Test.LeanCheck.Error (errorToFalse)
import Test.LeanCheck.Utils


-- | A single functional ingredient in conjuring.
-- Specify conjure ingredients with 'con' and 'fun':
--
-- > conjure "foo" foo [ con False
-- >                   , con True
-- >                   , con (0 :: Int)
-- >                   , con (1 :: Int)
-- >                   , ...
-- >                   , fun "&&" (&&)
-- >                   , fun "||" (||)
-- >                   , fun "+" ((+) :: Int -> Int -> Int)
-- >                   , fun "*" ((*) :: Int -> Int -> Int)
-- >                   , fun "-" ((-) :: Int -> Int -> Int)
-- >                   , ...
-- >                   ]
--
-- Ingredients may include arbitrary
-- constants ('con'),
-- constructors ('con')
-- or functions ('fun').
-- These may be built-in or user defined.
-- Use 'con' on 'Show' instances
-- and 'fun' otherwise.
--
-- This is internally
-- an arbitrary atomic 'Expr'ession
-- paired with
-- a 'Reification' of type information.
type Ingredient  =  (Expr, Reification)


-- | Provides a constant or constructor as an ingredient to Conjure.
--   To be used on 'Show' instances.
--   (cf. 'fun')
--
-- > conjure "foo" foo [ con False
-- >                   , con True
-- >                   , con (0 :: Int)
-- >                   , con (1 :: Int)
-- >                   , ...
-- >                   ]
--
-- Argument types have to be monomorphized,
-- so use type bindings when applicable.
con :: (Conjurable a, Show a) => a -> Ingredient
con x  =  (val x, conjureType x)


-- | Provides a functional value as an ingredient to Conjure.
--   To be used on values that are not 'Show' instances
--   such as functions.
--   (cf. 'con')
--
-- > conjure "foo" foo [ ...
-- >                   , fun "&&" (&&)
-- >                   , fun "||" (||)
-- >                   , fun "+" ((+) :: Int -> Int -> Int)
-- >                   , fun "*" ((*) :: Int -> Int -> Int)
-- >                   , fun "-" ((-) :: Int -> Int -> Int)
-- >                   , ...
-- >                   ]
--
-- Argument types have to be monomorphized,
-- so use type bindings when applicable.
fun :: Conjurable a => String -> a -> Ingredient
fun s x  =  (value s x, conjureType x)


-- | Provides an if condition bound to the given return type
--   as a Conjure ingredient.
--
-- This should be used when one wants Conjure to consider
-- if-expressions at all:
--
-- > last' :: [Int] -> Int
-- > last' [x]  =  x
-- > last' [x,y]  =  y
-- > last' [x,y,z]  =  z
--
-- > > conjure "last" last' [ con ([] :: [Int])
-- > >                      , fun ":" ((:) :: Int -> [Int] -> [Int])
-- > >                      , fun "null" (null :: [Int] -> Bool)
-- > >                      , iif (undefined :: Int)
-- > >                      , fun "undefined" (undefined :: Int)
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
iif :: Conjurable a => a -> Ingredient
iif x  =  (ifFor x, conjureType x)


-- | Provides a guard bound to the conjured function's return type.
--
-- Guards are only alllowed at the root fo the RHS.
--
-- > last' :: [Int] -> Int
-- > last' [x]  =  x
-- > last' [x,y]  =  y
-- > last' [x,y,z]  =  z
--
-- > > conjure "last" last'
-- > >   [ con ([] :: [Int])
-- > >   , fun ":" ((:) :: Int -> [Int] -> [Int])
-- > >   , fun "null" (null :: [Int] -> Bool)
-- > >   , guard
-- > >   , fun "undefined" (undefined :: Int)
-- > >   ]
-- > last :: [Int] -> Int
-- > -- 0.0s, testing 360 combinations of argument values
-- > -- 0.0s, pruning with 5/5 rules
-- > -- 0.0s, 1 candidates of size 1
-- > -- 0.0s, 0 candidates of size 2
-- > -- 0.0s, 0 candidates of size 3
-- > -- 0.0s, 0 candidates of size 4
-- > -- 0.0s, 0 candidates of size 5
-- > -- 0.0s, 0 candidates of size 6
-- > -- 0.0s, 4 candidates of size 7
-- > -- 0.0s, tested 2 candidates
-- > last []  =  undefined
-- > last (x:xs)
-- >   | null xs  =  x
-- >   | otherwise  =  last xs
guard :: Ingredient
guard  =  (guardFor (undefined :: Bool), conjureType (undefined :: Bool))
-- internally we always return a guard of the Bool return type,
-- this is replaced by Conjure when enumerating candidates


-- | Provides a case condition bound to the given return type.
--
-- This should be used when one wants Conjure to consider ord-case expressions:
--
-- > > conjure "mem" mem
-- > >   [ con False
-- > >   , con True
-- > >   , fun "`compare`" (compare :: Int -> Int -> Ordering)
-- > >   , ordcase (undefined :: Bool)
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
ordcase :: Conjurable a => a -> Ingredient
ordcase x  =  (caseForOrd x, conjureType x)


-- the following functions mirror their "conjure" counterparts from
-- Conjure.Conjurable but need a list of Ingredients instead of a Conjurable
-- representative.

-- | Computes a list of 'Reification1's from a list of 'Ingredient's.
--
-- This function mirrors functionality of 'conjureReification'.
cjReification :: [Ingredient] -> [Reification1]
cjReification ps  =  nubOn (\(eh,_,_,_,_,_) -> eh)
                  $  foldr (.) id (map snd ps) [conjureReification1 bool]

-- | Computes a list of holes encoded as 'Expr's from a list of 'Ingredient's.
--
-- This function mirrors functionality from 'conjureHoles'.
cjHoles :: [Ingredient] -> [Expr]
cjHoles ps  =  [eh | (eh,_,Just _,_,_,_) <- cjReification ps]

-- | Computes a function that equates two 'Expr's from a list of 'Ingredient's.
--
-- This function mirrors functionality from 'conjureMkEquation'.
cjMkEquation :: [Ingredient] -> Expr -> Expr -> Expr
cjMkEquation ps  =  mkEquation [eq | (_,Just eq,_,_,_,_) <- cjReification ps]

-- | Given a list of 'Ingredient's,
--   computes a function that checks whether two 'Expr's are equal
--   up to a given number of tests.
cjAreEqual :: [Ingredient] -> Int -> Expr -> Expr -> Bool
cjAreEqual ps maxTests  =  (===)
  where
  (-==-)  =  cjMkEquation ps
  e1 === e2  =  isTrue $ e1 -==- e2
  isTrue  =  all (errorToFalse . eval False) . gs
  gs  =  take maxTests . cjGrounds ps

-- | Given a list of 'Ingredient's,
--   computes a grounds function that lists
--   ground expressions of an 'Expr'.
cjGrounds :: [Ingredient] -> Expr -> [Expr]
cjGrounds  =  grounds . cjTiersFor

-- | Given a list of 'Ingredient's,
--   returns a function that given an 'Expr'
--   will return tiers of test 'Expr' values.
--
-- This is used in 'cjAreEqual'.
cjTiersFor :: [Ingredient] -> Expr -> [[Expr]]
cjTiersFor ps e  =  tf allTiers
  where
  allTiers :: [ [[Expr]] ]
  allTiers  =  [etiers | (_,_,Just etiers,_,_,_) <- cjReification ps]
  tf []  =  [[e]] -- no tiers found, keep variable
  tf (etiers:etc)  =  case etiers of
                      ((e':_):_) | typ e' == typ e -> etiers
                      _                            -> tf etc




-- | __DEPRACATED__.  Please use 'Ingredient' instead
type Prim  =  Ingredient
{-# DEPRECATED Prim "'Prim' is deprecated, please use 'Ingredient' instead" #-}


-- | __DEPRECATED__.  Please use 'con' instead.
pr :: (Conjurable a, Show a) => a -> Ingredient
pr  =  con
{-# DEPRECATED pr "'pr' is deprecated, please use 'con' instead" #-}

-- | __DEPRECATED__.  Please use 'fun' instead.
prim :: Conjurable a => String -> a -> Ingredient
prim  =  fun
{-# DEPRECATED prim "'prim' is deprecated, please use 'fun' instead" #-}

prif :: Conjurable a => a -> Ingredient
prif  =  iif
{-# DEPRECATED prif "'prif' is deprecated, please use 'iif' instead" #-}

primOrdCaseFor :: Conjurable a => a -> Ingredient
primOrdCaseFor  =  ordcase
{-# DEPRECATED primOrdCaseFor "'primOrdCaseFor' is deprecated, please use 'ordcase' instead" #-}
