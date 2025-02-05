-- print candidates
--
-- Copyright (C) 2021-2025 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure
import Conjure.Engine
import Conjure.Defn
import Data.Express.Fixtures

printCandidates :: Conjurable f => Int -> Int -> String -> f -> [Prim] -> IO ()
printCandidates m n nm f ps  =  do
  putStrLn $ "Candidates for: " ++ nm ++ " :: " ++ show (typeOf f)
  putStrLn $ "  pruning with " ++ show nRules ++ "/" ++ show nREs ++ " rules"
  putStrLn $ "  " ++ show (map length css1) ++ " direct candidates, " ++ show nd1 ++ " duplicates"
  putStrLn $ "  " ++ show (map length cssC) ++ " pattern candidates, " ++ show ndC ++ " duplicates"
  putStrLn ""
  printThy thy
  putStrLn $ "direct candidates:\n"
  putStrLn $ unlines $ map showDefn $ concat $ take n $ css1
  putStrLn $ "pattern candidates:\n"
  putStrLn $ unlines $ map showDefn $ concat $ take n $ cssC
  where
  nd1  =  length cs1 - length (nubSort cs1)
  ndC  =  length csC - length (nubSort csC)
  cs1  =  concat css1
  csC  =  concat cssC
  css1  =  take m css1'
  cssC  =  take m cssC'
  (css1', thy, _)  =  candidateDefns1 args nm f ps
  (cssC', _, _)    =  candidateDefnsC args nm f ps
  nRules  =  length (rules thy)
  nREs  =  length (equations thy) + nRules

main :: IO ()
main  =  do
  printCandidates 9 6 "foo" (undefined :: Int -> Int)
    [ pr (0 :: Int)
    , pr (1 :: Int)
    , prim "+" ((+) :: Int -> Int -> Int)
    , prim "*" ((*) :: Int -> Int -> Int)
    , prim "-" ((-) :: Int -> Int -> Int)
    ]

  printCandidates 9 5 "?" (undefined :: Int -> Int -> Int)
    [ pr (0 :: Int)
    , prim "+" ((+) :: Int -> Int -> Int)
    , prim "*" ((*) :: Int -> Int -> Int)
    , prim "dec" (subtract 1 :: Int -> Int)
    ]

  printCandidates 9 6 "goo" (undefined :: [Int] -> [Int])
    [ pr ([] :: [Int])
    , prim ":" ((:) :: Int -> [Int] -> [Int])
    , prim "++" ((++) :: [Int] -> [Int] -> [Int])
    ]

  printCandidates 9 6 "??" (undefined :: [Int] -> [Int] -> [Int])
    [ pr ([] :: [Int])
    , prim ":" ((:) :: Int -> [Int] -> [Int])
    , prim "++" ((++) :: [Int] -> [Int] -> [Int])
    ]

  printCandidates 9 6 "ton" (undefined :: Bool -> Bool)
    [ pr False
    , pr True
    , prim "&&" (&&)
    , prim "||" (||)
    , prim "not" not
    ]

  printCandidates 9 6 "&|" (undefined :: Bool -> Bool -> Bool)
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
  printCandidates 9 6 "gcd" (undefined :: Int -> Int -> Int)
    [ pr (0::Int)
    , prim "`mod`" (mod :: Int -> Int -> Int)
    ]
