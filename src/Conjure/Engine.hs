-- |
-- Module      : Conjure.Engine
-- Copyright   : (c) 2021 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- An internal module of 'Conjure',
-- a library for Conjuring function implementations
-- from tests or partial definitions.
-- (a.k.a.: functional inductive programming)
{-# LANGUAGE CPP, RecordWildCards, TupleSections #-}
module Conjure.Engine
  ( conjure
  , conjureWithMaxSize
  , Args(..)
  , args
  , conjureWith
  , conjpure
  , conjpureWith
  , candidateExprs
  , module Data.Express
  , module Data.Express.Fixtures
  , module Test.Speculate.Engine
  , module Test.Speculate.Reason
  )
where

import Control.Monad (when)

import Data.Express
import Data.Express.Fixtures hiding ((-==-))

import Test.LeanCheck
import Test.LeanCheck.Tiers
import Test.LeanCheck.Error (errorToTrue, errorToFalse, errorToNothing)

import Test.Speculate.Reason (isRootNormalE)
import Test.Speculate.Engine (theoryFromAtoms, groundBinds)

import Conjure.Expr
import Conjure.Conjurable


-- | Conjures an implementation of a partially defined function.
--
-- Takes a 'String' with the name of a function,
-- a partially-defined function from a conjurable type,
-- and a list of building blocks encoded as 'Expr's.
--
-- For example, given:
--
-- > square :: Int -> Int
-- > square 0  =  0
-- > square 1  =  1
-- > square 2  =  4
-- >
-- > primitives :: [Expr]
-- > primitives =
-- >   [ val (0::Int)
-- >   , val (1::Int)
-- >   , value "+" ((+) :: Int -> Int -> Int)
-- >   , value "*" ((*) :: Int -> Int -> Int)
-- > ]
--
-- The conjure function does the following:
--
-- > > conjure "square" square primitives
-- > square :: Int -> Int
-- > -- testing 3 combinations of argument values
-- > -- looking through 3 candidates of size 1
-- > -- looking through 3 candidates of size 2
-- > -- looking through 5 candidates of size 3
-- > square x  =  x * x
--
-- The primitives list is defined with 'val' and 'value'.
conjure :: Conjurable f => String -> f -> [Expr] -> IO ()
conjure  =  conjureWith args


-- | Like 'conjure' but allows setting the maximum size of considered expressions
--   instead of the default value of 12.
--
-- > conjureWithMaxSize 10 "function" function [...]
conjureWithMaxSize :: Conjurable f => Int -> String -> f -> [Expr] -> IO ()
conjureWithMaxSize sz  =  conjureWith args
                       {  maxSize = sz
                       ,  maxEquationSize = min sz (maxEquationSize args)
                       }


-- | Arguments to be passed to 'conjureWith' or 'conjpureWith'.
--   See 'args' for the defaults.
data Args = Args
  { maxTests          :: Int  -- ^ maximum number of tests to each candidate
  , maxSize           :: Int  -- ^ maximum size of candidate bodies
  , maxRecursiveCalls :: Int  -- ^ maximum number of allowed recursive calls
  , maxEquationSize   :: Int  -- ^ maximum size of equation operands
  , maxRecursionSize  :: Int  -- ^ maximum size of a recursive expression expansion
  , maxSearchTests    :: Int  -- ^ maximum number of tests to search for defined values
  , forceTests :: [[Expr]]  -- ^ force tests
  }


-- | Default arguments to conjure.
--
-- * 60 tests
-- * functions of up to 12 symbols
-- * maximum of 1 recursive call
-- * pruning with equations up to size 5
-- * recursion up to 60 symbols
-- * search for defined applications for up to 100000 combinations
args :: Args
args = Args
  { maxTests           =  60
  , maxSize            =  12
  , maxRecursiveCalls  =   1
  , maxEquationSize    =   5
  , maxRecursionSize   =  60
  , maxSearchTests     =  100000
  , forceTests         =  []
  }


-- | Like 'conjure' but allows setting options through 'Args'/'args'.
--
-- > conjureWith args{maxSize = 11} "function" function [...]
conjureWith :: Conjurable f => Args -> String -> f -> [Expr] -> IO ()
conjureWith args nm f es  =  do
  print (var (head $ words nm) f)
  putStrLn $ "-- testing " ++ show (length ts) ++ " combinations of argument values"
  pr 1 rs
  where
  pr n []  =  putStrLn $ "cannot conjure"
  pr n ((is,cs,es):rs)  =  do
    -- when (n==1) $ putStrLn $ unlines $ map show es
    putStrLn $ "-- looking through "
            ++ show (length cs) ++ "/" ++ show (length es)
            ++ " candidates of size " ++ show n
    case is of
      []     ->  pr (n+1) rs
      (i:_)  ->  do putStrLn $ showEq i
                    putStrLn ""
  rs  =  zip3 iss css ess
  (iss, css, ess, ts)  =  conjpureWith args nm f es


-- | Like 'conjure' but in the pure world.
--
-- Returns a triple with:
--
-- 1. tiers of implementations
-- 2. tiers of candidate bodies (right type)
-- 3. tiers of candidate expressions (any type)
-- 4. a list of tests
conjpure :: Conjurable f => String -> f -> [Expr] -> ([[Expr]], [[Expr]], [[Expr]], [Expr])
conjpure =  conjpureWith args


-- | Like 'conjpure' but allows setting options through 'Args' and 'args'.
conjpureWith :: Conjurable f => Args -> String -> f -> [Expr] -> ([[Expr]], [[Expr]], [[Expr]], [Expr])
conjpureWith Args{..} nm f es  =  (implementationsT, candidatesT, allCandidatesT, tests)
  where
  tests  =  [ffxx //- bs | bs <- dbss]
  implementationsT  =  mapT (vffxx -==-) $ filterT implements candidatesT
  implements e  =  apparentlyTerminates rrff e
                && ffxx ?=? recursexpr maxRecursionSize vffxx e
  candidatesT  =  filterT (\e -> typ e == typ ffxx) allCandidatesT
  allCandidatesT  =  take maxSize
                  $  candidateExprs nm f maxEquationSize maxRecursiveCalls (===) es
  ffxx   =  conjureApplication nm f
  vffxx  =  conjureVarApplication nm f
  (rrff:xxs)  =  unfoldApp vffxx

  (===)  =  conjureAreEqual f maxTests
  e1 ?=? e2  =  isTrueWhenDefined (e1 -==- e2)
  (-==-)  =  conjureMkEquation f

  isTrueWhenDefined e  =  all (errorToFalse . eval False) $ map (e //-) dbss

  bss, dbss :: [[(Expr,Expr)]]
  bss  =  take maxSearchTests $ groundBinds (conjureTiersFor f) ffxx
  fbss  =  [zip xxs vs | vs <- forceTests, isWellTyped $ foldApp (rrff:vs)]
  dbss  =  take maxTests
        $  ([bs | bs <- bss, errorToFalse . eval False $ e //- bs] \\ fbss)
        ++ fbss
    where
    e  =  ffxx -==- ffxx


candidateExprs :: Conjurable f
               => String -> f
               -> Int
               -> Int
               -> (Expr -> Expr -> Bool)
               -> [Expr]
               -> [[Expr]]
candidateExprs nm f sz mc (===) es  =
  enumerateApps keep $ nub $ (ef:exs) ++ es ++ [conjureIf f]
  where
  efxs  =  conjureVarApplication nm f
  (ef:exs)  =  unfoldApp efxs
  keep e  =  isRootNormalE thy e && count (== ef) (vars e) <= mc
  thy  =  theoryFromAtoms (===) sz . (:[]) . nub
       $  conjureHoles f ++ [val False, val True] ++ es ++ [conjureIf f]


candidatesTD :: (Expr -> Bool) -> Expr -> [Expr] -> [[Expr]]
candidatesTD keep h primitives  =  filterT (not . hasHole)
                                $  town [[h]]
  where
  most = mostGeneralCanonicalVariation

  town :: [[Expr]] -> [[Expr]]
  town ((e:es):ess) | keep (most e)  =  [[e]] \/ town (expand e \/ (es:ess))
                    | otherwise      =  town (es:ess)
  town ([]:ess)  =  []:town ess
  town []  =  []

  expand :: Expr -> [[Expr]]
  expand e  =  case holesBFS e of
    [] -> []
    (h:_) -> mapT (fillBFS e) (replacementsFor h)

  replacementsFor :: Expr -> [[Expr]]
  replacementsFor h  =  filterT (\e -> typ e == typ h)
                     $  primitiveApplications primitives
