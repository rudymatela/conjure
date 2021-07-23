-- |
-- Module      : Conjure.Prim
-- Copyright   : (c) 2021 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of 'Conjure'.
--
-- The 'Prim' type and utilities involving it.
--
-- You are probably better off importing "Conjure".
module Conjure.Prim
  ( Prim (..)
  , prim
  , pr
  , cjHoles
  , cjTiersFor
  , cjAreEqual
  , cjMkEquation
  )
where

import Conjure.Conjurable
import Conjure.Expr
import Conjure.Utils
import Test.LeanCheck.Error (errorToFalse)
import Test.LeanCheck.Utils
import Test.Speculate.Expr


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


-- the following functions mirror their "conjure" counterparts from
-- Conjure.Conjurable but need a list of Prims instead of a Conjurable
-- representative.

cjReification :: [Prim] -> [Reification1]
cjReification ps  =  nubOn (\(eh,_,_,_,_,_) -> eh)
                  $  foldr (.) id (map snd ps) [conjureReification1 bool]

cjHoles :: [Prim] -> [Expr]
cjHoles ps  =  [eh | (eh,_,Just _,_,_,_) <- cjReification ps]

cjMkEquation :: [Prim] -> Expr -> Expr -> Expr
cjMkEquation ps  =  mkEquation [eq | (_,Just eq,_,_,_,_) <- cjReification ps]

cjAreEqual :: [Prim] -> Int -> Expr -> Expr -> Bool
cjAreEqual ps maxTests  =  (===)
  where
  (-==-)  =  cjMkEquation ps
  e1 === e2  =  isTrue $ e1 -==- e2
  isTrue  =  all (errorToFalse . eval False) . gs
  gs  =  take maxTests . grounds (cjTiersFor ps)

cjTiersFor :: [Prim] -> Expr -> [[Expr]]
cjTiersFor ps e  =  tf allTiers
  where
  allTiers :: [ [[Expr]] ]
  allTiers  =  [etiers | (_,_,Just etiers,_,_,_) <- cjReification ps]
  tf []  =  [[e]] -- no tiers found, keep variable
  tf (etiers:etc)  =  case etiers of
                      ((e':_):_) | typ e' == typ e -> etiers
                      _                            -> tf etc
