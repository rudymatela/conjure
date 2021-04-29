-- |
-- Module      : Conjure
-- Copyright   : (c) 2021 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- A library for Conjuring function implementations
-- from tests or partial definitions.
-- (a.k.a.: functional inductive programming)
--
-- This is currently an experimental tool in its early stages,
-- don't expect much from its current version.
-- It is just a piece of curiosity in its current state.
--
-- Step 1: declare your partial function
--
-- > square :: Int -> Int
-- > square 0  =  0
-- > square 1  =  1
-- > square 2  =  4
--
-- Step 2: declare a list with the potential building blocks:
--
-- > background :: [Expr]
-- > background =
-- >   [ val (0::Int)
-- >   , val (1::Int)
-- >   , value "+" ((+) :: Int -> Int -> Int)
-- >   , value "*" ((*) :: Int -> Int -> Int)
-- > ]
--
-- Step 3: call conjure and see your generated function:
--
-- > > conjure "square" square background
-- > square :: Int -> Int
-- > -- looking through 815 candidates, 100% match, 3/3 assignments
-- > square x  =  x * x
{-# LANGUAGE CPP #-}
module Conjure
  (
-- * Basic use
    conjure
  , val
  , value
  , Expr

-- * Advanced use
  , conjpure
  , Args(..)
  , args
  , conjureWith
  , conjpureWith

  , Conjurable (conjureEquality, conjureTiers)
  , reifyEquality, reifyTiers
  )
where

import Conjure.Engine
import Conjure.Conjurable
