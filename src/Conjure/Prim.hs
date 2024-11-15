-- |
-- Module      : Conjure.Prim
-- Copyright   : (c) 2021-2024 Rudy Matela
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


-- | A primtive expression (paired with instance reification).
type Prim  =  (Expr, Reification)


-- | Provides a primitive value to Conjure.
--   To be used on 'Show' instances.
--   (cf. 'prim')
pr :: (Conjurable a, Show a) => a -> Prim
pr x  =  (val x, conjureType x)


-- | Provides a primitive value to Conjure.
--   To be used on values that are not 'Show' instances
--   such as functions.
--   (cf. 'pr')
prim :: Conjurable a => String -> a -> Prim
prim s x  =  (value s x, conjureType x)


-- | Provides an if condition bound to the given return type.
prif :: Conjurable a => a -> Prim
prif x  =  (ifFor x, conjureType x)


-- | Provides a case condition bound to the given return type.
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
