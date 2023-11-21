-- Print redundant candidates
--
-- Copyright (C) 2023 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure
import Conjure.Engine
import Conjure.Defn
import Conjure.Utils
import Data.Dynamic (fromDyn, dynApp)
import Data.Express.Fixtures


printRedundantCandidates :: Conjurable f => Int -> String -> f -> [Prim] -> IO ()
printRedundantCandidates n nm f ps  =  do
  putStrLn $ "Redundant candidates for: " ++ nm ++ " :: " ++ show (typeOf f)
  putStrLn $ "  pruning with " ++ show nRules ++ "/" ++ show nREs ++ " rules"
  putStrLn $ "  " ++ show (map length css) ++ " candidates"
  putStrLn $ "  " ++ show numUnique ++ "/" ++ show numCandidates ++ " unique candidates"
  putStrLn $ "  " ++ show numRedundant ++ "/" ++ show numCandidates ++ " redundant candidates"
  putStrLn ""
  printThy thy
  putStrLn $ unlines . map showClass $ filter (\xs -> length xs > 1) $ classes
  where
  numCandidates  =  length candidates
  numUnique      =  length classes
  numRedundant   =  numCandidates - numUnique
  classes        =  classifyBy (equalModuloTesting maxTests maxEvalRecursions nm f) candidates
  candidates     =  concat css
  css            =  take n css'
  (css', thy)    =  candidateDefnsC args nm f ps  -- Conjure uses this for listing candidates
  nRules         =  length (rules thy)
  nREs           =  length (equations thy) + nRules
  maxTests       = 360
  maxEvalRecursions = 60


-- shows a class of candidates
showClass :: [Defn] -> String
showClass cs  =  unlines $ heading : map (indent . showDefn) cs
  where
  heading  =  "class of " ++ show (length cs) ++ " equivalent candidates:\n"


main :: IO ()
main  =  do
  let n = 5

  printRedundantCandidates n "foo" (undefined :: Int -> Int)
    [ pr (0 :: Int)
    , pr (1 :: Int)
    , prim "+" ((+) :: Int -> Int -> Int)
    , prim "*" ((+) :: Int -> Int -> Int)
    , prim "-" ((-) :: Int -> Int -> Int)
    ]

  printRedundantCandidates n "?" (undefined :: Int -> Int -> Int)
    [ pr (0 :: Int)
    , prim "+" ((+) :: Int -> Int -> Int)
    , prim "*" ((+) :: Int -> Int -> Int)
    , prim "dec" (subtract 1 :: Int -> Int)
    ]

  printRedundantCandidates n "goo" (undefined :: [Int] -> [Int])
    [ pr ([] :: [Int])
    , prim ":" ((:) :: Int -> [Int] -> [Int])
    , prim "++" ((++) :: [Int] -> [Int] -> [Int])
    ]

  printRedundantCandidates n "??" (undefined :: [Int] -> [Int] -> [Int])
    [ pr ([] :: [Int])
    , prim ":" ((:) :: Int -> [Int] -> [Int])
    , prim "++" ((++) :: [Int] -> [Int] -> [Int])
    ]

  printRedundantCandidates n "ton" (undefined :: Bool -> Bool)
    [ pr False
    , pr True
    , prim "&&" (&&)
    , prim "||" (||)
    , prim "not" not
    ]

  printRedundantCandidates n "&|" (undefined :: Bool -> Bool -> Bool)
    [ pr False
    , pr True
    , prim "&&" (&&)
    , prim "||" (||)
    , prim "not" not
    ]

  -- Degenerate case:
  -- lots of redundancy, since 0 `mod` 0 = undefined
  -- Speculate is not able to discover that x `mod` x = 0
  -- nevertheless useful for observing candidate filtering
  -- through other means
  {-
  printRedundantCandidates n "gcd" (undefined :: Int -> Int -> Int)
    [ pr (0::Int)
    , prim "`mod`" (mod :: Int -> Int -> Int)
    ]
  -}
