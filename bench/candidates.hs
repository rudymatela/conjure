-- print candidates
--
-- Copyright (C) 2021 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure
import Conjure.Engine
import Conjure.Defn
import Data.Express.Fixtures

printCandidates :: Conjurable f => Int -> Int -> String -> f -> [Prim] -> IO ()
printCandidates m n nm f ps  =  do
  putStrLn $ "Candidates for: " ++ nm ++ " :: " ++ show (typeOf f)
  putStrLn $ "  pruning with " ++ show nRules ++ "/" ++ show nREs ++ " rules"
  putStrLn $ "  " ++ show (map length cs1) ++ " direct candidates"
  putStrLn $ "  " ++ show (map length csC) ++ " pattern candidates"
  putStrLn ""
  printThy thy
  putStrLn $ "direct candidates:\n"
  putStrLn $ unlines $ map showDefn $ concat $ take n $ cs1
  putStrLn $ "pattern candidates:\n"
  putStrLn $ unlines $ map showDefn $ concat $ take n $ csC
  where
  cs1  =  take m cs1'
  csC  =  take m csC'
  (cs1', thy)  =  candidateDefns1 args nm f ps
  (csC', _)    =  candidateDefnsC args nm f ps
  nRules  =  length (rules thy)
  nREs  =  length (equations thy) + nRules

main :: IO ()
main  =  do
  printCandidates 9 6 "foo" (undefined :: Int -> Int)
    [ pr (0 :: Int)
    , pr (1 :: Int)
    , prim "+" ((+) :: Int -> Int -> Int)
    , prim "*" ((+) :: Int -> Int -> Int)
    ]

  printCandidates 6 4 "?" (undefined :: Int -> Int -> Int)
    [ pr (0 :: Int)
    , prim "+" ((+) :: Int -> Int -> Int)
    , prim "*" ((+) :: Int -> Int -> Int)
    ]

  printCandidates 9 6 "goo" (undefined :: [Int] -> [Int])
    [ pr ([] :: [Int])
    , prim ":" ((:) :: Int -> [Int] -> [Int])
    , prim "++" ((++) :: [Int] -> [Int] -> [Int])
    ]

  printCandidates 6 4 "??" (undefined :: [Int] -> [Int] -> [Int])
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
