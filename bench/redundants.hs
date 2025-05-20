-- Print redundant candidates
--
-- Copyright (C) 2023-2025 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure

-- This script needs some internal utilities of Conjure:
import Conjure.Engine
import Conjure.Defn
import Conjure.Defn.Redundancy
import Conjure.Defn.Test
import Conjure.Utils
import Test.LeanCheck.Tiers (discardT)


-- | This function prints redundant candidates.
--
-- The arguments are, in their respective order:
--
-- * maximum candidate size (5 = few seconds, 7 = few minutes);
-- * function name (for pretty-printing purposes);
-- * proxy value to indicate the type of functions to generate;
-- * list of ingredients, in Conjure-compatible form.
printRedundantCandidates :: Conjurable f => Int -> String -> f -> [Ingredient] -> IO ()
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
  css            =  take n
                 .  discardT isRedundantByIntroduction -- additional pruning rule
                 $  css'
  (css', thy, _, _) =  candidateDefns nm f ps  -- Conjure uses this for listing candidates
  nRules         =  length (rules thy)
  nREs           =  length (equations thy) + nRules
  maxTests       =  60 -- a hardcoded value probably will not hurt in this simple benchmark
  maxEvalRecursions  =  30

  -- shows a class of candidates
  showClass :: [Defn] -> String
  showClass cs  =  unlines $ heading : map (indent . showDefn) cs
    where
    heading  =  "class of " ++ show (length cs) ++ " equivalent candidates:\n"


main :: IO ()
main  =  do

  -- This N value limits the maximum size of candidates,
  -- increase it to print redundant candidates of bigger size.
  let n = 5
  -- With n = 5, this should run in a few seconds.
  -- With n = 6, this should run in a few more seconds.
  -- With n = 7 and -O2 optimization,
  -- this should take a few minutes to run and generate a ~300K text file.
  -- We can also customize the n per-function below:

  printRedundantCandidates (n+1) "foo" (undefined :: Int -> Int)
    [ unfun (0 :: Int)
    , unfun (1 :: Int)
    , fun "+" ((+) :: Int -> Int -> Int)
    , fun "*" ((*) :: Int -> Int -> Int)
    , fun "-" ((-) :: Int -> Int -> Int)
    ]

  printRedundantCandidates n "?" (undefined :: Int -> Int -> Int)
    [ unfun (0 :: Int)
    , fun "+" ((+) :: Int -> Int -> Int)
    , fun "*" ((*) :: Int -> Int -> Int)
    , fun "dec" (subtract 1 :: Int -> Int)
    ]

  printRedundantCandidates (n+1) "goo" (undefined :: [Int] -> [Int])
    [ unfun ([] :: [Int])
    , fun ":" ((:) :: Int -> [Int] -> [Int])
    , fun "++" ((++) :: [Int] -> [Int] -> [Int])
    ]

  printRedundantCandidates n "??" (undefined :: [Int] -> [Int] -> [Int])
    [ unfun ([] :: [Int])
    , fun ":" ((:) :: Int -> [Int] -> [Int])
    , fun "++" ((++) :: [Int] -> [Int] -> [Int])
    ]

  printRedundantCandidates (n+1) "ton" (undefined :: Bool -> Bool)
    [ unfun False
    , unfun True
    , fun "&&" (&&)
    , fun "||" (||)
    , fun "not" not
    ]

  printRedundantCandidates n "&|" (undefined :: Bool -> Bool -> Bool)
    [ unfun False
    , unfun True
    , fun "&&" (&&)
    , fun "||" (||)
    , fun "not" not
    ]

  -- Degenerate case:
  -- lots of redundancy, since 0 `mod` 0 = undefined
  -- Speculate is not able to discover that x `mod` x = 0
  -- nevertheless useful for observing candidate filtering
  -- through other means
  {-
  printRedundantCandidates n "gcd" (undefined :: Int -> Int -> Int)
    [ unfun (0::Int)
    , fun "`mod`" (mod :: Int -> Int -> Int)
    ]
  -}
