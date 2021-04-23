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

  , mainTest
  )
where

import System.Exit (exitFailure)
import System.Environment (getArgs)

import Conjure
import Conjure.Expr hiding (delete, insert)

reportTests :: [Bool] -> IO ()
reportTests tests =
  case elemIndices False tests of
    [] -> putStrLn "Tests passed!"
    is -> do putStrLn ("Failed tests:" ++ show is)
             exitFailure

getMaxTestsFromArgs :: Int -> IO Int
getMaxTestsFromArgs n = do
  as <- getArgs
  return $ case as of
             (s:_) -> read s
             _     -> n

mainTest :: (Int -> [Bool]) -> Int -> IO ()
mainTest tests n' = do
  n <- getMaxTestsFromArgs n'
  reportTests (tests n)

printLines :: Show a => [a] -> IO ()
printLines = putStrLn . unlines . map show
