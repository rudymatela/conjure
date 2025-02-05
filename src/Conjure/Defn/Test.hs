-- |
-- Module      : Conjure.Defn.Test
-- Copyright   : (c) 2021-2025 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of "Conjure".
--
-- This module exports functions that test 'Defn's using ground values.
--
-- You are probably better off importing "Conjure".
module Conjure.Defn.Test
  ( equalModuloTesting
  , erroneousCandidate
  , findDefnError
  , listDefnErrors
  )
where

import Conjure.Defn
import Conjure.Conjurable

import Test.LeanCheck.Error (errorToFalse, errorToNothing)

import Data.Dynamic (fromDyn, dynApp)


equalModuloTesting :: Conjurable f => Int -> Int -> String -> f -> Defn -> Defn -> Bool
equalModuloTesting maxTests maxEvalRecursions nm f  =  (===)
  where
  testGrounds  =  nonNegativeAppGrounds maxTests maxEvalRecursions nm f
  d1 === d2  =  all are $ testGrounds
    where
    -- silences errors, ok since this is for optional measuring of optimal pruning
    are :: Expr -> Bool
    are e  =  isError (ee d1 d1 e)
           && isError (ee d2 d2 e)
           || errorToFalse (ee d1 d2 e)
           where  ee  =  devlEqual maxEvalRecursions f


-- | For debugging purposes.
--
-- This may be taken out of the API at any moment.
erroneousCandidate :: Conjurable f => Int -> Int -> String -> f -> Defn -> Bool
erroneousCandidate maxTests maxEvalRecursions nm f  =
  isJust . findDefnError maxTests maxEvalRecursions nm f


-- | For debugging purposes,
--   finds a set of arguments that triggers an error in the candidate 'Defn'.
--
-- Warning: this is an experimental function
-- which may be taken out of the API at any moment.
findDefnError :: Conjurable f => Int -> Int -> String -> f -> Defn -> Maybe Expr
findDefnError maxTests maxEvalRecursions nm f  =
  listToMaybe . listDefnErrors maxTests maxEvalRecursions nm f


listDefnErrors :: Conjurable f => Int -> Int -> String -> f -> Defn -> [Expr]
listDefnErrors maxTests maxEvalRecursions nm f d  =  filter is testGrounds
  where
  testGrounds :: [Expr]
  testGrounds  =  nonNegativeAppGrounds maxTests maxEvalRecursions nm f
  is :: Expr -> Bool
  is e  =  isError (devlEqual maxEvalRecursions f d d e)


nonNegativeAppGrounds :: Conjurable f => Int -> Int -> String -> f -> [Expr]
nonNegativeAppGrounds maxTests maxEvalRecursions nm f
  =  filter (none isNegative . unfoldApp)
  $  take maxTests
  $  conjureGrounds f (conjureVarApplication nm f)


devlEqual :: Conjurable f => Int -> f -> Defn -> Defn -> Expr -> Bool
devlEqual maxEvalRecursions f d1 d2 e  =
  eq `dynApp` evalDyn d1 e
     `dynApp` evalDyn d2 e `fromDyn` err
  where
  evalDyn d e  =  fromMaybe err (toDynamicWithDefn (conjureExpress f) maxEvalRecursions d e)
  eq  =  conjureDynamicEq f
  err  =  error "Conjure.devlEqual: evaluation error"
  -- We cannot use conjureMkEquation here because
  -- we need different Defns at each side of the equation
  -- so they have to be evaluated independently.

-- | Is the argument value an error value?
isError :: a -> Bool
isError  =  isNothing . errorToNothing
-- There should not be a problem if this ever appears in LeanCheck:
-- imports are qualfied in this module.
