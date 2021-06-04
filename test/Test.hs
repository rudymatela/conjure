-- |
-- Module      : Test
-- Copyright   : (c) 2021 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Internal test module for Conjure,
-- intended to be imported from all test programs and only by them.
module Test
  ( module Conjure
  , module Conjure.Expr
  , module Conjure.Conjurable
  , module Test.LeanCheck
  , module Test.LeanCheck.Utils
  , module Test.ListableExpr
  , module Test.Candidates

  , mainTest
  )
where

import System.Exit (exitFailure)
import System.Environment (getArgs, getProgName)

import Test.LeanCheck
import Test.LeanCheck.Utils
import Test.ListableExpr
import Test.Candidates

import Conjure
import Conjure.Expr hiding (delete, insert)
import Conjure.Conjurable

reportTests :: String -> [Bool] -> IO ()
reportTests s tests = do
  case elemIndices False tests of
    [] -> putStrLn $ s ++ ": tests passed"
    is -> do putStrLn (s ++ ": failed tests: " ++ show is)
             exitFailure

getMaxTestsFromArgs :: Int -> IO Int
getMaxTestsFromArgs n = do
  as <- getArgs
  return $ case as of
             (s:_) -> read s
             _     -> n

mainTest :: (Int -> [Bool]) -> Int -> IO ()
mainTest tests n' = do
  pn <- getProgName
  n <- getMaxTestsFromArgs n'
  reportTests pn (tests n)

printLines :: Show a => [a] -> IO ()
printLines = putStrLn . unlines . map show
