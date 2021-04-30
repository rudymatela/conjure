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
  , nubOn
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

nubOn :: Eq b => (a -> b) -> [a] -> [a]
nubOn f  =  nubBy ((==) `on` f)
