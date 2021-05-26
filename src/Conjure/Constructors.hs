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
  )
where

import Data.Express
import Data.Express.Express
import Data.Typeable (Typeable)

class Typeable a => Constructors a where
  constructors :: a -> [Expr]

instance Constructors () where
  constructors _  =  [val ()]

instance Constructors Bool where
  constructors _  =  [val False, val True]

constructorsNum :: (Show a, Num a, Typeable a) => a -> [Expr]
constructorsNum x  =  [ val (0 -: x)
                      , value "inc" ((+1) ->: x)
                      ]

instance Constructors Int where
  constructors  =  constructorsNum

instance Constructors Integer where
  constructors  =  constructorsNum

instance Constructors Char where
  constructors _  =  []

instance (Typeable a, Show a) => Constructors [a] where
  constructors xs  =  [ val ([] -: xs)
                      , value ":" ((:) ->>: xs)
                      ]

instance ( Typeable a, Show a
         , Typeable b, Show b
         ) => Constructors (a,b) where
  constructors xy  =  [value "," ((,) ->>: xy)]

instance ( Typeable a, Show a
         , Typeable b, Show b
         , Typeable c, Show c
         ) => Constructors (a,b,c) where
  constructors xyz  =  [value ",," ((,,) ->>>: xyz)]

instance Typeable a => Constructors (Maybe a) where
  constructors mx  =  [ value "Nothing" (Nothing -: mx)
                      , value "Just" (Just ->: mx)
                      ]

instance (Typeable a, Typeable b) => Constructors (Either a b) where
  constructors exy  =  [ value "Left" (Left ->: exy)
                       , value "Right" (Right ->: exy)
                       ]
