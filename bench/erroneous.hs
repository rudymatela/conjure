-- Print erroneous candidates
--
-- Copyright (C) 2024 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure

-- This script needs some internal utilities of Conjure:
import Conjure.Engine
import Conjure.Defn
import Conjure.Defn.Redundancy
import Conjure.Defn.Test
import Conjure.Utils
import Test.LeanCheck.Tiers (discardT)


-- | This function prints erroneous candidates,
--   i.e.: candidates that yield errors or loop indefinitely
--         even if for just a single combination of argument values.
--
-- The arguments are, in their respective order:
--
-- * maximum candidate size (5 = few seconds, 7 = few minutes);
-- * function name (for pretty-printing purposes);
-- * proxy value to indicate the type of functions to generate;
-- * list of primitives, in Conjure-compatible form.
printErroneousCandidates :: Conjurable f => Int -> String -> f -> [Prim] -> IO ()
printErroneousCandidates n nm f ps  =  do
  putStrLn $ "Erroneous candidates for: " ++ nm ++ " :: " ++ show (typeOf f)
  putStrLn $ "  pruning with " ++ show nRules ++ "/" ++ show nREs ++ " rules"
  putStrLn $ "  " ++ show (map length css) ++ " candidates"
  putStrLn $ "  " ++ show numErroneous ++ "/" ++ show numCandidates ++ " erroneous candidates"
  putStrLn ""
--printThy thy
  putStrLn $ unlines . map showDefnWithError $ erroneous
  where
  numCandidates  =  length candidates
  numErroneous   =  length erroneous
  erroneous      =  [(c, e) | c <- candidates, Just e <- [findError c]]
  candidates     =  concat css
  css            =  take n
                 .  discardT isRedundantByIntroduction -- additional pruning rule
                 $  css'
  (css', thy)    =  candidateDefnsC args nm f ps  -- Conjure uses this for listing candidates
  nRules         =  length (rules thy)
  nREs           =  length (equations thy) + nRules
  maxTests       =  60 -- a hardcoded value probably will not hurt in this simple benchmark
  maxEvalRecursions  =  60
  findError      =  findDefnError maxTests maxEvalRecursions nm f
  showDefnWithError (d,e)  =  showDefn d
                           ++ "-- " ++ showExpr e ++ "  =  bottom\n"


main :: IO ()
main  =  do

  -- This N value limits the maximum size of candidates,
  -- increase it to print erroneous candidates of bigger size.
  let n = 7

  printErroneousCandidates n "foo" (undefined :: Int -> Int)
    [ pr (0 :: Int)
    , pr (1 :: Int)
    , prim "+" ((+) :: Int -> Int -> Int)
    , prim "*" ((+) :: Int -> Int -> Int)
    , prim "-" ((-) :: Int -> Int -> Int)
    ]

  printErroneousCandidates n "?" (undefined :: Int -> Int -> Int)
    [ pr (0 :: Int)
    , prim "+" ((+) :: Int -> Int -> Int)
    , prim "*" ((+) :: Int -> Int -> Int)
    , prim "dec" (subtract 1 :: Int -> Int)
    ]

  printErroneousCandidates n "goo" (undefined :: [Int] -> [Int])
    [ pr ([] :: [Int])
    , prim ":" ((:) :: Int -> [Int] -> [Int])
    , prim "++" ((++) :: [Int] -> [Int] -> [Int])
    ]

  printErroneousCandidates n "??" (undefined :: [Int] -> [Int] -> [Int])
    [ pr ([] :: [Int])
    , prim ":" ((:) :: Int -> [Int] -> [Int])
    , prim "++" ((++) :: [Int] -> [Int] -> [Int])
    ]

  printErroneousCandidates n "ton" (undefined :: Bool -> Bool)
    [ pr False
    , pr True
    , prim "&&" (&&)
    , prim "||" (||)
    , prim "not" not
    ]

  printErroneousCandidates n "&|" (undefined :: Bool -> Bool -> Bool)
    [ pr False
    , pr True
    , prim "&&" (&&)
    , prim "||" (||)
    , prim "not" not
    ]
