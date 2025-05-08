-- |
-- Module      : Conjure.Settings
-- Copyright   : (c) 2021-2025 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- An internal module of "Conjure",
-- a library for Conjuring function implementations
-- from tests or partial definitions.
-- (a.k.a.: functional inductive programming)
--
-- This contains the settings for functions in "Conjure.Engine".
{-# LANGUAGE CPP, RecordWildCards, TupleSections #-}
module Conjure.Settings
  ( Args(..)
  , args
  )
where


-- | Arguments to be passed to 'conjureWith' or 'conjpureWith'.
--   See 'args' for the defaults.
data Args = Args
  { maxTests              :: Int  -- ^ maximum number of tests to each candidate
  , maxSize               :: Int  -- ^ maximum size of candidate bodies
  , target                :: Int  -- ^ enumerate further sizes of candidates until this target
  , maxEvalRecursions     :: Int  -- ^ maximum number of recursive evaluations when testing candidates
  , maxEquationSize       :: Int  -- ^ maximum size of equation operands
  , maxSearchTests        :: Int  -- ^ maximum number of tests to search for defined values
  , maxDeconstructionSize :: Int  -- ^ maximum size of deconstructions (e.g.: @_ - 1@)
  , maxConstantSize       :: Int  -- ^ maximum size of constants (0 for no limit)
  , maxPatternSize        :: Int  -- ^ maximum size of patterns (0 for no limit)
  , maxPatternDepth       :: Int  -- ^ maximum depth of patterns

  -- advanced & debug options --
  , carryOn               :: Bool -- ^ whether to carry on after finding a suitable candidate
  , showTheory            :: Bool -- ^ show theory discovered by Speculate used in pruning
  , usePatterns           :: Bool -- ^ use pattern matching to create (recursive) candidates
  , showRuntime           :: Bool -- ^ show runtime
  , showCandidates        :: Bool -- ^ (debug) show candidates -- warning: wall of text
  , showTests             :: Bool -- ^ (debug) show tests
  , showPatterns          :: Bool -- ^ (debug) show possible LHS patterns
  , showDeconstructions   :: Bool -- ^ (debug) show conjectured-and-allowed deconstructions

  -- pruning options --
  , rewriting             :: Bool -- ^ unique-modulo-rewriting candidates
  , requireDescent        :: Bool -- ^ require recursive calls to deconstruct arguments
  , adHocRedundancy       :: Bool -- ^ ad-hoc redundancy checks
  , copyBindings          :: Bool -- ^ copy partial definition bindings in candidates
  , earlyTests            :: Bool -- ^ perform tests early-and-independently on each binding
  , atomicNumbers         :: Bool -- ^ restrict constant/ground numeric expressions to atoms
  , requireZero           :: Bool -- ^ require 0 as base case for Num recursions
  , uniqueCandidates      :: Bool -- ^ unique-modulo-testing candidates
  }


-- | Default arguments to conjure.
--
-- * 60 tests
-- * functions of up to 24 symbols
-- * target testing over 50400 candidates
-- * maximum of one recursive call allowed in candidate bodies
-- * maximum evaluation of up to 60 recursions
-- * pruning with equations up to size 5
-- * search for defined applications for up to 100000 combinations
-- * require recursive calls to deconstruct arguments
-- * don't show the theory used in pruning
-- * do not show tested candidates
-- * do not make candidates unique module testing
args :: Args
args = Args
  { maxTests               =  360
  , maxSize                =  24
  , target                 =  10080
  , maxEvalRecursions      =  60
  , maxEquationSize        =   5
  , maxSearchTests         =  110880
  , maxDeconstructionSize  =   4
  , maxConstantSize        =   0 -- unlimited
  , maxPatternSize         =   0 -- unlimited
  , maxPatternDepth        =   1

  -- advanced & debug options --
  , carryOn                =  False
  , showTheory             =  False
  , usePatterns            =  True
  , showRuntime            =  True
  , showCandidates         =  False
  , showTests              =  False
  , showDeconstructions    =  False
  , showPatterns           =  False

  -- pruning options --
  , rewriting              =  True
  , requireDescent         =  True
  , adHocRedundancy        =  True
  , copyBindings           =  True
  , earlyTests             =  True
  , atomicNumbers          =  True
  , requireZero            =  False
  , uniqueCandidates       =  False
  }

-- TODO: remove the requireZero option from args?
