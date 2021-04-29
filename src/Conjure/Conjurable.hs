-- |
-- Module      : Conjure.Conjurable
-- Copyright   : (c) 2021 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of 'Conjure'.
--
-- This defines the 'Conjurable' typeclass
-- and utilities involving it.
--
-- You are probably better off importing "Conjure".
module Conjure.Conjurable
  ( Reification1
  , Reification
  , Conjurable (..)
  , conjureType
  , canonicalApplication
  , canonicalVarApplication
  , conjureTiersFor
  )
where

import Test.LeanCheck
import Test.LeanCheck.Utils
import Conjure.Expr hiding (application)
import Test.Speculate.Expr


-- | Single reification of some functions over a type as 'Expr's.
--
-- A hole, an if function, an equality function and tiers.
type Reification1  =  (Expr, Expr, Maybe Expr, Maybe [[Expr]])

-- | A reification over a collection of types.
--
-- Represented as a transformation of a list to a list.
type Reification  =  [Reification1] -> [Reification1]


class Typeable a => Conjurable a where
  argumentHoles :: a -> [Expr]
  argumentHoles _  =  []

  conjureEquality :: a -> Maybe Expr
  conjureEquality _  =  Nothing

  conjureTiers :: a -> Maybe [[Expr]]
  conjureTiers _  =  Nothing

  conjureSubTypes :: a -> Reification
  conjureSubTypes _  =  id


conjureType :: Conjurable a => a -> Reification
conjureType x ms  =
  if hole x `elem` [h | (h,_,_,_) <- ms]
  then ms
  else conjureSubTypes x $ (hole x, ifFor x, conjureEquality x, conjureTiers x) : ms

reifyEquality :: (Eq a, Typeable a) => a -> Maybe Expr
reifyEquality  =  Just . head . reifyEq

reifyTiers :: (Listable a, Show a, Typeable a) => a -> Maybe [[Expr]]
reifyTiers  =  Just . mkExprTiers

mkExprTiers :: (Listable a, Show a, Typeable a) => a -> [[Expr]]
mkExprTiers a  =  mapT val (tiers -: [[a]])

conjureTiersFor :: Conjurable f => f -> Expr -> [[Expr]]
conjureTiersFor f e  =  tf allTiers
  where
  allTiers :: [ [[Expr]] ]
  allTiers  =  mkExprTiers (undefined :: Bool)
            :  [etiers | (_,_,_,Just etiers) <- conjureType f []]
  tf []  =  [[e]] -- no tiers found, keep variable
  tf (etiers:etc)  =  case etiers of
                      ((e':_):_) | typ e' == typ e -> etiers
                      _                            -> tf etc


instance Conjurable () where
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

instance Conjurable Int where
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

instance Conjurable Integer where
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

instance Conjurable Bool where
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

-- TODO: remove Eq restriction here and throughout
instance (Conjurable a, Listable a, Show a, Eq a) => Conjurable [a] where
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers
  conjureSubTypes xs  =  conjureType (head xs)


instance ( Conjurable a, Listable a, Show a, Eq a
         , Conjurable b, Listable b, Show b, Eq b
         ) => Conjurable (a,b) where
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers
  conjureSubTypes xy  =  conjureType (fst xy)
                      .  conjureType (snd xy)

instance (Conjurable a, Listable a, Show a, Eq a) => Conjurable (Maybe a) where
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers
  conjureSubTypes xs  =  conjureType (fromJust xs)

instance ( Conjurable a, Listable a, Show a, Eq a
         , Conjurable b, Listable b, Show b, Eq b
         ) => Conjurable (Either a b) where
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers
  conjureSubTypes xs  =  conjureType (fromLeft xs)
                      .  conjureType (fromRight xs)

instance Conjurable Float where
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

instance Conjurable Double where
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

instance Conjurable Ordering where
  conjureEquality  =  reifyEquality
  conjureTiers     =  reifyTiers

-- TODO: complete the following instances
instance (Typeable a, Typeable b, Typeable c) => Conjurable (a,b,c)
instance (Typeable a, Typeable b, Typeable c, Typeable d) => Conjurable (a,b,c,d)
instance (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e) => Conjurable (a,b,c,d,e)
instance (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f) => Conjurable (a,b,c,d,e,f)

instance (Listable a, Name a, Show a, Conjurable a, Conjurable b) => Conjurable (a -> b) where
  argumentHoles f  =  hole (argTy f) : argumentHoles (f undefined)
  conjureSubTypes f  =   conjureType (argTy f) . conjureType (resTy f)

argTy :: (a -> b) -> a
argTy _  =  undefined

resTy :: (a -> b) -> b
resTy _  =  undefined

canonicalArgumentVariables :: Conjurable f => f -> [Expr]
canonicalArgumentVariables  =  unfoldApp
                            .  mostGeneralCanonicalVariation
                            .  foldApp
                            .  argumentHoles

canonicalApplication :: Conjurable f => String -> f -> Expr
canonicalApplication nm f  =  foldApp (value nm f : canonicalArgumentVariables f)

canonicalVarApplication :: Conjurable f => String -> f -> Expr
canonicalVarApplication nm f  =  foldApp (var nm f : canonicalArgumentVariables f)
