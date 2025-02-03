-- Print unique candidates
--
-- Copyright (C) 2023-2024 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
--
-- This can be used to track
-- if we loose any solutions from implementing pruning techniques.
import Conjure

-- This script needs some internal utilities of Conjure:
import Conjure.Engine
import Conjure.Defn
import Conjure.Defn.Redundancy
import Conjure.Defn.Test
import Conjure.Utils
import Test.LeanCheck.Tiers (discardT, discardLaterT)


-- | This function prints unique candidates.
--
-- The arguments are, in their respective order:
--
-- * maximum candidate size (5 = few seconds, 7 = few minutes);
-- * function name (for pretty-printing purposes);
-- * proxy value to indicate the type of functions to generate;
-- * list of primitives, in Conjure-compatible form.
printUniqueCandidates :: Conjurable f => Int -> String -> f -> [Prim] -> IO ()
printUniqueCandidates n nm f ps  =  do
  putStrLn $ "Unique candidates for: " ++ nm ++ " :: " ++ show (typeOf f)
  putStrLn $ "  pruning with " ++ show nRules ++ "/" ++ show nREs ++ " rules"
  putStrLn $ "  " ++ show (map length css) ++ " candidates"
  putStrLn $ "  " ++ show (map length uss) ++ " unique candidates"
  putStrLn $ "  " ++ show numUnique ++ "/" ++ show numCandidates ++ " unique candidates"
  putStrLn $ "  " ++ show numRedundant ++ "/" ++ show numCandidates ++ " redundant candidates"
  putStrLn ""
  printThy thy
  putStrLn $ unlines . map showDefn $ unique
  where
  numCandidates  =  length candidates
  numUnique      =  length unique
  numRedundant   =  numCandidates - numUnique
  unique         =  concat uss
  candidates     =  concat css
  uss            =  discardLaterT (equalModuloTesting maxTests maxEvalRecursions nm f)
                 $  css
  css            =  take n
                 $  discardT isRedundantByIntroduction -- additional pruning rule
                 $  css'
  (css', thy, _) =  candidateDefnsC args nm f ps  -- Conjure uses this for listing candidates
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
  let n = 5

  printUniqueCandidates (n+2) "foo" (undefined :: Int -> Int)
    [ pr (0 :: Int)
    , pr (1 :: Int)
    , prim "+" ((+) :: Int -> Int -> Int)
    , prim "*" ((*) :: Int -> Int -> Int)
    , prim "-" ((-) :: Int -> Int -> Int)
    ]

  printUniqueCandidates n "?" (undefined :: Int -> Int -> Int)
    [ pr (0 :: Int)
    , prim "+" ((+) :: Int -> Int -> Int)
    , prim "*" ((*) :: Int -> Int -> Int)
    , prim "dec" (subtract 1 :: Int -> Int)
    ]

  printUniqueCandidates (n+2) "goo" (undefined :: [Int] -> [Int])
    [ pr ([] :: [Int])
    , prim ":" ((:) :: Int -> [Int] -> [Int])
    , prim "++" ((++) :: [Int] -> [Int] -> [Int])
    ]

  printUniqueCandidates n "??" (undefined :: [Int] -> [Int] -> [Int])
    [ pr ([] :: [Int])
    , prim ":" ((:) :: Int -> [Int] -> [Int])
    , prim "++" ((++) :: [Int] -> [Int] -> [Int])
    ]

  printUniqueCandidates (n+2) "ton" (undefined :: Bool -> Bool)
    [ pr False
    , pr True
    , prim "&&" (&&)
    , prim "||" (||)
    , prim "not" not
    ]

  printUniqueCandidates n "&|" (undefined :: Bool -> Bool -> Bool)
    [ pr False
    , pr True
    , prim "&&" (&&)
    , prim "||" (||)
    , prim "not" not
    ]
