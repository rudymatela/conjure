-- |
-- Module      : Conjure.Conjurable
-- Copyright   : (c) 2021 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of 'Conjure'.
--
-- This defines the 'Conjurable' and 'Arguable' typeclasses
-- and utilities involving it.
--
-- You are probably better off importing "Conjure".
module Conjure.Conjurable
  ( Conjurable
  , canonicalApplication
  , canonicalVarApplication
  , unifiedArgumentTiers
  , tiersFor
  , mkExprTiers
  )
where

import Test.LeanCheck
import Test.LeanCheck.Utils
import Conjure.Expr hiding (application)
import Test.Speculate.Expr

class Typeable a => Conjurable a where
  argumentHoles :: a -> [Expr]
  argumentHoles _  =  []
  argumentTiers :: a -> [ [[Expr]] ]
  argumentTiers _  =  []

instance Conjurable Int
instance Conjurable Bool
instance Conjurable a => Conjurable [a]

instance (Listable a, Name a, Show a, Typeable a, Conjurable b) => Conjurable (a -> b) where
  argumentHoles f  =  hole (arg1 f) : argumentHoles (f undefined)
  argumentTiers f  =  mkExprTiers (arg1 f) : argumentTiers (f undefined)

arg1 :: (a -> b) -> a
arg1 _  =  undefined

canonicalArgumentVariables :: Conjurable f => f -> [Expr]
canonicalArgumentVariables  =  unfoldApp
                            .  mostGeneralCanonicalVariation
                            .  foldApp
                            .  argumentHoles

canonicalApplication :: Conjurable f => String -> f -> Expr
canonicalApplication nm f  =  foldApp (value nm f : canonicalArgumentVariables f)

canonicalVarApplication :: Conjurable f => String -> f -> Expr
canonicalVarApplication nm f  =  foldApp (var nm f : canonicalArgumentVariables f)

unifiedArgumentTiers :: Conjurable f => f -> [[Expr]]
unifiedArgumentTiers  =  foldr (\/) [] . nubArgumentTiers

nubArgumentTiers :: Conjurable f => f -> [[ [Expr] ]]
nubArgumentTiers  =  nubOn tierepr . argumentTiers
  where
  nubOn f  =  nubBy ((==) `on` f)
  -- NOTE: this is O(n*n),
  -- not much of a problem the number of arguments will hardly pass 6.

mkExprTiers :: (Listable a, Show a, Typeable a) => a -> [[Expr]]
mkExprTiers a  =  mapT val (tiers -: [[a]])

tiersFor :: Conjurable f => f -> Expr -> [[Expr]]
tiersFor f e  =  tf (mkExprTiers (undefined :: Bool) : argumentTiers f)
  where
  tf []  =  [[e]] -- no tiers found, keep variable
  tf (etiers:etc)  =  case etiers of
                      ((e':_):_) | typ e' == typ e -> etiers
                      _                            -> tf etc

-- | tries to extract a representative from the first 6 tiers
tierepr :: [[Expr]] -> Maybe Expr
tierepr ((e:_):_)                 =  Just e
tierepr ([]:(e:_):_)              =  Just e
tierepr ([]:[]:(e:_):_)           =  Just e
tierepr ([]:[]:[]:(e:_):_)        =  Just e
tierepr ([]:[]:[]:[]:(e:_):_)     =  Just e
tierepr ([]:[]:[]:[]:[]:(e:_):_)  =  Just e
tierepr _                         =  Nothing
