-- |
-- Module      : Conjure.Cases
-- Copyright   : (c) 2021 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of 'Conjure'.
--
-- This module defines the 'Cases' typeclass
-- that allows listing cases of a type
-- encoded as 'Expr's
--
-- You are probably better off importing "Conjure".
{-# Language DeriveDataTypeable, StandaloneDeriving #-} -- for GHC < 7.10
module Conjure.Cases
  ( Cases (..)
  , Fxpr
  , sumFxpr
  , factFxpr
  , nullFxpr
  , isZeroFxpr
  )
where

import Conjure.Utils
import Data.Express
import Data.Express.Express
import Data.Express.Fixtures
import Data.Dynamic
import Data.Typeable (Typeable)

type Fxpr  =  (Expr, Cxpr)
type Cxpr  =  [([Expr],Expr)]

sumFxpr :: Fxpr
sumFxpr  =  var "sum" (undefined :: [Int] -> Int) =-
  [ [nil]           =-  zero
  , [(xx -:- xxs)]  =-  xx -+- (var "recurse" (undefined :: [Int] -> Int) :$ xxs)
  ]
  where
  (=-) = (,)
  infixr 0 =-

factFxpr :: Fxpr
factFxpr  =  error "TODO: write me"

nullFxpr :: Fxpr
nullFxpr  =  error "TODO" =-
  [ [nil]          =- false
  , [(xx -:- xxs)] =- false
  ]
  where
  (=-) = (,)
  infixr 0 =-

isZeroFxpr :: Fxpr
isZeroFxpr  =  error "TODO" =-
  [ [zero]  =- true
  , [inc xx] =- false
  ]
  where
  inc = undefined -- TODO: define me
  (=-) = (,)
  infixr 0 =-


-- | Evaluates an 'Expr' using the given 'Fxpr' as definition
--   when a recursive call is found.
fxprToDynamic :: Int -> Fxpr -> Expr -> Maybe Dynamic
fxprToDynamic  =  undefined


class Express a => Cases a where
  cases :: a -> [Expr]

instance Cases () where
  cases _  =  [val ()]

instance Cases Bool where
  cases _  =  [val False, val True]

instance Cases Int where
  cases _  =  []

instance Cases Integer where
  cases _  =  []

instance Cases Char where
  cases _  =  []

instance Express a => Cases [a] where
  cases xs  =  [ val ([] -: xs)
               , value ":" ((:) ->>: xs) :$ hole x :$ hole xs
               ]
    where
    x  =  head xs

instance (Express a, Express b) => Cases (a,b) where
  cases xy  =  [value "," ((,) ->>: xy) :$ hole x :$ hole y]
    where
    (x,y) = (undefined,undefined) -: xy

instance (Express a, Express b, Express c) => Cases (a,b,c) where
  cases xyz  =  [value ",," ((,,) ->>>: xyz) :$ hole x :$ hole y :$ hole z]
    where
    (x,y,z) = (undefined,undefined,undefined) -: xyz

instance Express a => Cases (Maybe a) where
  cases mx  =  [ value "Nothing" (Nothing -: mx)
               , value "Just" (Just ->: mx) :$ hole x
               ]
    where
    x  =  Just undefined -: mx


instance (Express a, Express b) => Cases (Either a b) where
  cases exy  =  [ value "Left" (Left ->: exy) :$ hole x
                , value "Right" (Right ->: exy) :$ hole y
                ]
    where
    x  =  Left undefined -: exy
    y  =  Right undefined -: exy
