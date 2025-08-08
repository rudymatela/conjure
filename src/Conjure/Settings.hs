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
  (
  -- * Basic settings
    maxTests
  , maxSize
  , target

  -- * Advanced settings
  , maxRecursions
  , maxEquationSize
  , maxSearchTests
  , maxDeconstructionSize
  , maxConstantSize
  , maxPatternSize
  , maxPatternDepth
  , undefinedIngredient

  -- * Debug options
  , showCandidates
  , showTheory
  , singlePattern
  , showTests
  , showPatterns
  , showDeconstructions
  , carryOn

  -- * Pruning options
  , dontRewrite
  , dontRequireDescent
  , omitAssortedPruning
  , maxEarlyTests
  , dontCopyBindings
  , nonAtomicNumbers
  , uniqueCandidates

  -- * Filtering settings
  , actual

  -- * Read basic settings
  , maxTestsI
  , targetAndMaxSizeI

  -- * Read advanced settings
  , maxRecursionsI
  , maxEquationSizeI
  , maxSearchTestsI
  , maxDeconstructionSizeI
  , maxConstantSizeI
  , maxPatternSizeI
  , maxPatternDepthI
  , undefinedIngredientI

  -- * Read debug options
  , showCandidatesI
  , showTheoryI
  , singlePatternI
  , showTestsI
  , showPatternsI
  , showDeconstructionsI
  , carryOnI

  -- * Read pruning options
  , rewriteI
  , requireDescentI
  , assortedPruningI
  , maxEarlyTestsI
  , copyBindingsI
  , atomicNumbersI
  , uniqueCandidatesI
  )
where


import Conjure.Utils
import Data.Express (val, eval, typ)
import Conjure.Ingredient (Ingredient)


-- | Arguments to be passed to
--   'Conjure.conjureWith' or 'Conjure.conjpureWith'.
--   You should use smart constructors instead.
data Setting
  = Noop                       -- ^ internal use: no-op setting

  | MaxTests              Int  -- ^ maximum number of tests to each candidate
  | MaxSize               Int  -- ^ maximum size of candidate bodies
  | Target                Int  -- ^ enumerate further sizes of candidates until this target
  | MaxRecursions         Int  -- ^ maximum number of recursive evaluations when testing candidates
  | MaxEquationSize       Int  -- ^ maximum size of equation operands
  | MaxSearchTests        Int  -- ^ maximum number of tests to search for defined values
  | MaxDeconstructionSize Int  -- ^ maximum size of deconstructions (e.g.: @- 1@)
  | MaxConstantSize       Int  -- ^ maximum size of constants (0 for no limit)
  | MaxPatternSize        Int  -- ^ maximum size of patterns (0 for no limit)
  | MaxPatternDepth       Int  -- ^ maximum depth of patterns

  -- special ingredients --
  | UndefinedIngredient        -- ^ allows undefined as the value for an equation

  -- advanced & debug options --
  | CarryOn              -- ^ carry on after finding a suitable candidate
  | ShowTheory           -- ^ show theory discovered by Speculate used in pruning
  | SinglePattern        -- ^ restrict candidates to a single pattern
  | ShowCandidates       -- ^ (debug) show candidates -- warning: wall of text
  | ShowTests            -- ^ (debug) show tests
  | ShowPatterns         -- ^ (debug) show possible LHS patterns
  | ShowDeconstructions  -- ^ (debug) show conjectured-and-allowed deconstructions

  -- pruning options --
  | DontRewrite          -- ^ turns off unique-modulo-rewriting candidates
  | DontRequireDescent   -- ^ require recursive calls to deconstruct arguments
  | OmitAssortedPruning  -- ^ omit other assorted pruning rules
  | MaxEarlyTests Int    -- ^ don't perform tests early-and-independently on each binding
  | DontCopyBindings     -- ^ don't copy partial definition bindings in candidates
  | AtomicNumbers        -- ^ restrict constant/ground numeric expressions to atoms
  | NonAtomicNumbers     -- ^ lift constant/ground numetic expression restrictions
  | UniqueCandidates     -- ^ unique-modulo-testing candidates
  deriving (Eq, Ord, Show, Read)


-- | Constructs an ingredient from a setting
setting :: Setting -> Ingredient
setting x  =  (val x, error "Conjure.Settings: evaluating reification, this is a bug")
  -- using 'id' instead of 'error' above would work,
  -- but we want to be warned in case we accidentally evaluate the reification

extract :: Ingredient -> Setting
extract  =  eval Noop . fst

-- | Lists actual incredients in the list
actual :: [Ingredient] -> [Ingredient]
actual is  =  [i | i <- is, typ (fst i) /= typeOf Noop]

-- | By default,
-- 'Conjure.conjure' tests candidates up to a maximum of 360 tests.
-- This configures the maximum number of tests to each candidate,
-- when provided in the list of ingredients:
--
-- > conjure "..." ... [ ...
-- >                   , maxTests 1080
-- >                   , ... ]
maxTests :: Int -> Ingredient
maxTests =  setting . MaxTests

-- | Finds the set maximum number of tests or set the default of 360
maxTestsI  :: [Ingredient] -> Int
maxTestsI is  =  headOr 360 [m | MaxTests m <- map extract is]
  -- the use of magic numbers goes well with the theme of Conjure.

-- | By default,
-- 'Conjure.conjure' imposes no limit on the size of candidates.
--
-- This configures a different maximum
-- when provided in the list of ingredients.
--
-- If only one of 'maxSize' and 'target' is defined,
-- it is used.  If none, target is used.
maxSize :: Int -> Ingredient
maxSize =  setting . MaxSize

-- | By default, 'Conjure.conjure' targets testing 10080 candidates.
-- This configures a different target when
-- provided in the list of ingredients:
--
-- > conjure "..." ... [ ...
-- >                   , target 5040
-- >                   , ... ]
target :: Int -> Ingredient
target =  setting . Target

-- | Computes the target and maxSize.
--
-- When none is provided, we default to a target of 10080.
targetAndMaxSizeI :: [Ingredient] -> (Int, Int)
targetAndMaxSizeI is  =
  case (t, m) of
  (0, 0) -> (10080, 0)
  (t, m) -> (t, m)
  where
  t  =  headOr 0 [m | Target m  <- map extract is]
  m  =  headOr 0 [m | MaxSize m <- map extract is]
-- above is a perfect use for the These datatype,
-- one of my favourite non-standard,
-- but I don't want to impose a dependency on my users...

-- | By default,
-- 'Conjure.conjure' evaluates candidates for up to 60 recursive calls.
--
-- This allows overriding the default
-- when provided in the ingredient list.
maxRecursions :: Int -> Ingredient
maxRecursions =  setting . MaxRecursions

maxRecursionsI :: [Ingredient] -> Int
maxRecursionsI is  =  headOr 60 [m | MaxRecursions m <- map extract is]


-- | By default,
-- 'Conjure.conjure' considers equations of up to 5 symbols
-- for pruning-through-rewriting.
--
-- This allows overriding the default:
-- 6 or 7 are also good values for this depending on the number of ingredients.
--
-- > conjure ... ... [ ...
-- >                 , maxEquationSize 6
-- >                 , ... ]
--
-- Internally, this is the maximum size passed to the Speculate tool.
maxEquationSize :: Int -> Ingredient
maxEquationSize =  setting . MaxEquationSize

maxEquationSizeI :: [Ingredient] -> Int
maxEquationSizeI is  =  headOr 5 [m | MaxEquationSize m <- map extract is]


-- | By default,
-- 'Conjure.conjure' enumerates up to 110880 argument combinations
-- while reifying the partial definition passed by the user.
--
-- This allows configuring a higher default
-- when provided in the ingredient list.
--
-- Increasing this setting is useful
-- when the partial definition is not exercised enough.
maxSearchTests :: Int -> Ingredient
maxSearchTests =  setting . MaxSearchTests

maxSearchTestsI :: [Ingredient] -> Int
maxSearchTestsI is  =  headOr 110880 [m | MaxSearchTests m <- map extract is]

-- | By default
-- 'Conjure.conjure' allows deconstruction expressions
-- of up to 4 symbols.
--
-- This allows overriding the default
-- when provided in the ingredient list.
maxDeconstructionSize :: Int -> Ingredient
maxDeconstructionSize =  setting . MaxDeconstructionSize

maxDeconstructionSizeI :: [Ingredient] -> Int
maxDeconstructionSizeI is  =  headOr 4 [m | MaxDeconstructionSize m <- map extract is]

-- | Configures a maximum size of constant sub-expressions
-- when provided in the ingredient list
-- of 'Conjure.conjure' or 'Conjure.conjureFromSpec'.
maxConstantSize :: Int -> Ingredient
maxConstantSize =  setting . MaxConstantSize

maxConstantSizeI :: [Ingredient] -> Int
maxConstantSizeI is  =  headOr 0 [m | MaxConstantSize m <- map extract is]

-- | By default,
-- 'Conjure.conjure' places no limit in the LHS pattern sizes.
--
-- This allows configuring a limit when provided in the ingredient list
maxPatternSize :: Int -> Ingredient
maxPatternSize =  setting . MaxPatternSize

maxPatternSizeI :: [Ingredient] -> Int
maxPatternSizeI is  =  headOr 0 [m | MaxPatternSize m <- map extract is]

-- | By default,
-- 'Conjure.conjure' enumerates pattern breakdowns of the outernmost constructor
-- of depth 1.
--
-- This allows overriding the default when provided in the ingredient list:
-- a depth of 2 allows breakdowns of the two outernmost constructors;
-- a depth of 3, three outernmost constructors;
-- etc.
maxPatternDepth :: Int -> Ingredient
maxPatternDepth =  setting . MaxPatternDepth

maxPatternDepthI :: [Ingredient] -> Int
maxPatternDepthI is  =  headOr 1 [m | MaxPatternDepth m <- map extract is]

-- | Carry on after finding a suitable candidate.
--   To be provided as a setting in the list of ingredients.
carryOn :: Ingredient
carryOn =  setting CarryOn

carryOnI :: [Ingredient] -> Bool
carryOnI is  =  notNull [True | CarryOn <- map extract is]

-- | (Debug option).
--   Shows the underlying theory used in pruning
--   when this is provided in the ingredient list.
showTheory :: Ingredient
showTheory =  setting ShowTheory

showTheoryI :: [Ingredient] -> Bool
showTheoryI is  =  notNull [True | ShowTheory <- map extract is]

-- | (Debug option)
--   When provided in the ingredient list,
--   this reverts to a legacy enumeration that
--   contains candidates with a single LHS matching everything.
singlePattern :: Ingredient
singlePattern =  setting SinglePattern

singlePatternI :: [Ingredient] -> Bool
singlePatternI is  =  notNull [True | SinglePattern <- map extract is]

-- | (Debug option)
-- When provided in the ingredients list,
-- this enables showing enumerated candidates.
--
-- > conjure ... ... [ ...
-- >                 , showCandidates
-- >                 , ... ]
--
-- Warning: activating this will likely produce a humongous wall-of-text.
showCandidates :: Ingredient
showCandidates =  setting ShowCandidates

showCandidatesI :: [Ingredient] -> Bool
showCandidatesI is  =  notNull [True | ShowCandidates <- map extract is]

-- | (Debug option)
-- When provided in the ingredients list,
-- 'Conjure.conjure' will print the tests reified from the partial definition.
-- (cf. 'maxTests', 'maxSearchTests')
showTests :: Ingredient
showTests =  setting ShowTests

showTestsI :: [Ingredient] -> Bool
showTestsI is  =  notNull [True | ShowTests <- map extract is]

-- | (Debug option)
-- When this option is provided in the ingredients list,
-- 'Conjure.conjure' will print the enumrated LHS patterns.
-- (cf. 'maxPatternSize', 'maxPatternDepth')
showPatterns :: Ingredient
showPatterns =  setting ShowPatterns

showPatternsI :: [Ingredient] -> Bool
showPatternsI is  =  notNull [True | ShowPatterns <- map extract is]

-- | (Debug option)
-- Makes 'Conjure.conjure' print enumerated deconstructions
-- when provided in its ingredient list.
showDeconstructions :: Ingredient
showDeconstructions =  setting ShowDeconstructions

showDeconstructionsI :: [Ingredient] -> Bool
showDeconstructionsI is  =  notNull [True | ShowDeconstructions <- map extract is]

-- | Disables rewriting-as-pruning
--   when provided in the ingredient list
--   of 'Conjure.conjure' or 'Conjure.conjureFromSpec'.
dontRewrite :: Ingredient
dontRewrite  =  setting DontRewrite

rewriteI :: [Ingredient] -> Bool
rewriteI is  =  null [False | DontRewrite <- map extract is]

-- | Disables the recursive descent requirement
--   when provided in the ingredient list
--   of 'Conjure.conjure' or 'Conjure.conjureFromSpec'.
dontRequireDescent :: Ingredient
dontRequireDescent  =  setting DontRequireDescent

requireDescentI :: [Ingredient] -> Bool
requireDescentI is  =  null [False | DontRequireDescent <- map extract is]

-- | Disables assorted pruning rules
--   when provided in the ingredient list
--   of 'Conjure.conjure' or 'Conjure.conjureFromSpec'.
omitAssortedPruning :: Ingredient
omitAssortedPruning  =  setting OmitAssortedPruning

assortedPruningI :: [Ingredient] -> Bool
assortedPruningI is  =  null [False | OmitAssortedPruning <- map extract is]

-- | Sets the maximum number of early tests
--   performed independently bindings/equations
--   when provided in the ingredient list
--   of 'Conjure.conjure' or 'Conjure.conjureFromSpec'.
--
--   When not set, this defaults to a modest 12.
maxEarlyTests :: Int -> Ingredient
maxEarlyTests  =  setting . MaxEarlyTests

maxEarlyTestsI :: [Ingredient] -> Int
maxEarlyTestsI is  =  headOr 12 [m | MaxEarlyTests m <- map extract is]

-- | Disables the copy-bindings rule
--   when provided in the ingredient list
--   of 'Conjure.conjure' or 'Conjure.conjureFromSpec'.
dontCopyBindings :: Ingredient
dontCopyBindings  =  setting DontCopyBindings

copyBindingsI :: [Ingredient] -> Bool
copyBindingsI is  =  null [False | DontCopyBindings <- map extract is]

atomicNumbers :: Ingredient
atomicNumbers =  setting AtomicNumbers

-- | Disables the requirement of atomic numeric expressions
--   when provided in the ingredient list
--   of 'Conjure.conjure' or 'Conjure.conjureFromSpec'.
--   (cf. 'maxConstantSize')
nonAtomicNumbers :: Ingredient
nonAtomicNumbers =  setting NonAtomicNumbers

atomicNumbersI :: [Ingredient] -> Bool
atomicNumbersI is  =  null [False | NonAtomicNumbers <- map extract is]

-- | Enables expensive unique-modulo-testing candidates
--   when provided in the ingredient list
--   of 'Conjure.conjure' or 'Conjure.conjureFromSpec'.
--
-- Warning: this makes 'Conjure.conjure' very slow,
-- it is only intended for approximating the theoretical
-- limits of pruning in toy examples.
uniqueCandidates :: Ingredient
uniqueCandidates =  setting UniqueCandidates

uniqueCandidatesI :: [Ingredient] -> Bool
uniqueCandidatesI is  =  notNull [True | UniqueCandidates <- map extract is]

undefinedIngredient :: Ingredient
undefinedIngredient  =  setting UndefinedIngredient

undefinedIngredientI :: [Ingredient] -> Bool
undefinedIngredientI is  =  notNull [True | UndefinedIngredient <- map extract is]
