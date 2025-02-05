-- |
-- Module      : Conjure
-- Copyright   : (c) 2021-2024 Rudy Matela
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
-- > primitives :: [Prim]
-- > primitives =
-- >   [ pr (0::Int)
-- >   , pr (1::Int)
-- >   , prim "+" ((+) :: Int -> Int -> Int)
-- >   , prim "*" ((*) :: Int -> Int -> Int)
-- >   ]
--
-- Step 3: call conjure and see your generated function:
--
-- > > conjure "square" square primitives
-- > square :: Int -> Int
-- > -- testing 3 combinations of argument values
-- > -- pruning with 14/25 rules
-- > -- looking through 3 candidates of size 1
-- > -- looking through 4 candidates of size 2
-- > -- looking through 9 candidates of size 3
-- > square x  =  x * x
{-# LANGUAGE CPP #-}
module Conjure
  (
-- * Basic use
    conjure
  , Prim
  , pr
  , prim
  , prif
  , primOrdCaseFor

-- * Advanced use
  , conjureWithMaxSize
  , conjureWith
  , Args(..)
  , args
  , Expr
  , val
  , value

-- * Conjuring from a specification
  , conjureFromSpec
  , conjureFromSpecWith

-- * When using custom types
  , Conjurable (conjureExpress, conjureEquality, conjureTiers, conjureCases, conjureSubTypes, conjureSize)
  , reifyExpress
  , reifyEquality
  , reifyTiers
  , conjureType
  , Name (..)
  , Express (..)
  , deriveConjurable
  , deriveConjurableIfNeeded
  , deriveConjurableCascading

-- * Pure interfaces
  , Results (..)
  , conjpure
  , conjpureWith

-- * Helper test types
  , A, B, C, D, E, F
  )
where

import Conjure.Engine
import Conjure.Conjurable
import Conjure.Prim
import Conjure.Conjurable.Derive
