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
  ( module Data.Express
  , module Data.Express.Fixtures
  , module Test.Speculate.Engine
  , module Test.Speculate.Reason
  , conjure
  , conjureWith
  , conjpure
  , conjpureWith
  , candidateExprs
  , ifFor
  )
where

import Data.Express
import Data.Express.Fixtures hiding ((-==-))
import qualified Data.Ratio
import Test.LeanCheck.Error (errorToTrue, errorToFalse, errorToNothing)
import Test.Speculate hiding ((===), Args(..), args)
import Test.Speculate.Reason
import Test.Speculate.Engine
import Test.Speculate.Expr
import System.IO

import Conjure.Expr
import Conjure.Conjurable

data Args = Args
  { maxTests         :: Int  -- ^ defaults to 60
  , maxSize          :: Int  -- ^ defaults to 9, keep greater than maxEquationSize
  , maxEquationSize  :: Int  -- ^ defaults to 5, keep smaller than maxSize
  , maxRecursionSize :: Int  -- ^ defaults to 60
  }

args :: Args
args = Args
  { maxTests          =  60
  , maxSize           =   9
  , maxEquationSize   =   5
  , maxRecursionSize  =  60
  }

conjpure :: Conjurable f => String -> f -> [Expr] -> (Int,Int,[(Int,Expr)])
conjpure =  conjpureWith args

conjpureWith :: Conjurable f => Args -> String -> f -> [Expr] -> (Int,Int,[(Int,Expr)])
conjpureWith Args{..} nm f es  =  (length candidates,totalDefined,) $ sortBy compareResult
  [ (ffxx .=. re, ffxx -==- e)
  | e <- candidates
  , apparentlyTerminates rrff e
  , let re = recursexpr maxRecursionSize vffxx e
  , ffxx ?=? re
  ]
  where
  totalDefined  =  ffxx .=. ffxx
  candidates  =  filter (\e -> typ e == typ ffxx)
              .  concat
              .  take maxSize
              $  candidateExprs nm f maxEquationSize (===) [es]
  ffxx   =  canonicalApplication nm f
  vffxx  =  canonicalVarApplication nm f
  rrff   =  var nm f

  (===), (?=?) :: Expr -> Expr -> Bool
  e1 === e2  =  isReallyTrue      (e1 -==- e2)
  e1 ?=? e2  =  isTrueWhenDefined (e1 -==- e2)

  e1 .=. e2  =  countTrue         (e1 -==- e2)
  (-==-)  =  mkEquation  eqs
    where
    eqs  =  value "==" ((==) :: Bool -> Bool -> Bool)
         :  es

  isTrueWhenDefined  =  all (errorToTrue  . eval False) . gs
  isReallyTrue       =  all (errorToFalse . eval False) . gs
  countTrue        =  count (errorToFalse . eval False) . gs

  gs :: Expr -> [Expr]
  gs  =  take maxTests . grounds (tiersFor f)

conjure :: Conjurable f => String -> f -> [Expr] -> IO ()
conjure  =  conjureWith args

conjureWith :: Conjurable f => Args -> String -> f -> [Expr] -> IO ()
conjureWith args nm f es  =  do
  print (var nm f)
  putStr $ "-- looking through " ++ show ncs ++ " candidates"
  hFlush stdout
  case rs of
    []    -> putStrLn $ "\ncannot conjure"
    ((n,e):_) -> do putStrLn $ ", " ++ showMatch n
                    putStrLn $ showEq e
--  nes -> putStrLn . unlines $ "":[showEq e ++ "  -- " ++ show n | (n,e) <- nes]
  putStrLn ""
  where
  (ncs,t,rs)  =  conjpureWith args nm f es
  showMatch n  =  show (n % t) ++ "% match, " ++ show n ++ "/" ++ show t ++ " assignments"
  showEq eq  =  showExpr (lhs eq) ++ "  =  " ++ showExpr (rhs eq)

candidateExprs :: Conjurable f
               => String -> f
               -> Int
               -> (Expr -> Expr -> Bool)
               -> [[Expr]]
               -> [[Expr]]
candidateExprs nm f sz (===) ess  =  expressionsT $ [ef:exs] \/ ess
  where
  (ef:exs)  =  unfoldApp $ canonicalVarApplication nm f
  thy  =  theoryFromAtoms (===) sz $ [nub (b_:map holeAsTypeOf exs)] \/ ess
  expressionsT ds  =  filterT (\e -> count (== ef) (vars e) <= 1)
                   $  filterT (isRootNormalE thy)
                   $  ds \/ (delay $ productMaybeWith ($$) es es)
    where
    es = expressionsT ds

lhs, rhs :: Expr -> Expr
lhs (((Value "==" _) :$ e) :$ _)  =  e
rhs (((Value "==" _) :$ _) :$ e)  =  e

compareResult :: (Int,Expr) -> (Int,Expr) -> Ordering
compareResult (n1,e1) (n2,e2)  =  n2 `compare` n1
                               <> e1 `compareSimplicity` e2

(%) :: Int -> Int -> Int
x % y  =  x * 100 `div` y
