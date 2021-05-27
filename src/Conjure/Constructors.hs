-- |
-- Module      : Conjure.Constructors
-- Copyright   : (c) 2021 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of 'Conjure'.
--
-- This module defines the 'Constructors' typeclass
-- that allows listing constructors of a type
-- encoded as 'Expr's
--
-- You are probably better off importing "Conjure".
{-# Language DeriveDataTypeable, StandaloneDeriving #-} -- for GHC < 7.10
module Conjure.Constructors
  ( Constructors (..)
  , Fxpr (..)
  , fxprExample
  )
where

import Data.Express
import Data.Express.Express
import Data.Express.Fixtures
import Data.Typeable (Typeable)

data Fxpr =  Fxpr [(Expr,Expr)]  deriving (Eq, Ord)

instance Show Fxpr where
  show (Fxpr [])  =  "# empty definition #\n"
  show (Fxpr bs)  =
    unlines $ show f:[showExpr pat ++ "  =  " ++ showExpr exp | (pat,exp) <- bs]
    where
    (f:exs)  =  unfoldApp . fst $ head bs

okExpr :: Expr -> Bool
okExpr  =  error "TODO: write me"

fxprExample :: Fxpr
fxprExample  =  Fxpr
  [ sum' nil           =-  zero
  , sum' (xx -:- xxs)  =-  xx -+- sum' xxs
  ]
  where
  (=-) = (,)
  infixr 0 =-

fvaluate :: Fxpr -> Maybe a
fvaluate  =  error "TODO: write me"

class Express a => Constructors a where
  constructors :: a -> [Expr]

instance Constructors () where
  constructors _  =  [val ()]

instance Constructors Bool where
  constructors _  =  [val False, val True]

constructorsNum :: (Num a, Express a) => a -> [Expr]
constructorsNum x  =  [ val (0 -: x)
                      , value "inc" ((+1) ->: x) :$ hole x
                      ]

instance Constructors Int where
  constructors  =  constructorsNum

instance Constructors Integer where
  constructors  =  constructorsNum

instance Constructors Char where
  constructors _  =  []

instance Express a => Constructors [a] where
  constructors xs  =  [ val ([] -: xs)
                      , value ":" ((:) ->>: xs) :$ hole x :$ hole xs
                      ]
    where
    x  =  head xs

instance (Express a, Express b) => Constructors (a,b) where
  constructors xy  =  [value "," ((,) ->>: xy) :$ hole x :$ hole y]
    where
    (x,y) = (undefined,undefined) -: xy

instance (Express a, Express b, Express c) => Constructors (a,b,c) where
  constructors xyz  =  [value ",," ((,,) ->>>: xyz) :$ hole x :$ hole y :$ hole z]
    where
    (x,y,z) = (undefined,undefined,undefined) -: xyz

instance Express a => Constructors (Maybe a) where
  constructors mx  =  [ value "Nothing" (Nothing -: mx)
                      , value "Just" (Just ->: mx) :$ hole x
                      ]
    where
    x  =  Just undefined -: mx

instance (Express a, Express b) => Constructors (Either a b) where
  constructors exy  =  [ value "Left" (Left ->: exy) :$ hole x
                       , value "Right" (Right ->: exy) :$ hole y
                       ]
    where
    x  =  Left undefined -: exy
    y  =  Right undefined -: exy
