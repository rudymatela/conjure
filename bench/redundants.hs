-- Print redundant candidates
--
-- Copyright (C) 2023 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure
import Conjure.Engine
import Conjure.Defn
import Conjure.Conjurable
import Conjure.Utils
import Test.LeanCheck.Stats (classifyBy)
import Test.LeanCheck.Error (errorToFalse)
import Data.Dynamic (fromDyn, dynApp)
import Data.Express.Fixtures


classifyCandidates :: Conjurable f => String -> f -> [Defn] -> [[Defn]]
classifyCandidates nm f  =  classifyBy (===)
  where
  err  =  error "nubCandidates: unexpected evaluation error."
  eq  =  conjureDynamicEq f
  d1 === d2  =  all are $ take maxTests $ grounds (conjureTiersFor f) (conjureVarApplication nm f)
    where
    are :: Expr -> Bool
    are e  =  errorToFalse -- silences errors, ok here since we are interested in uniqueness modulo testing
           $  (`fromDyn` err)
           $  eq `dynApp` fromMaybe err (toDynamicWithDefn (conjureExpress f) maxEvalRecursions d1 e)
                 `dynApp` fromMaybe err (toDynamicWithDefn (conjureExpress f) maxEvalRecursions d2 e)
  -- some hardcoded values
  maxEvalRecursions = 60
  maxTests = 360


printRedundantCandidates :: Conjurable f => Int -> String -> f -> [Prim] -> IO ()
printRedundantCandidates n nm f ps  =  do
  putStrLn $ "Redundant candidates for: " ++ nm ++ " :: " ++ show (typeOf f)
  putStrLn $ "  pruning with " ++ show nRules ++ "/" ++ show nREs ++ " rules"
  putStrLn $ "  " ++ show (map length css) ++ " candidates"
  putStrLn $ "  " ++ show numUnique ++ "/" ++ show numCandidates ++ " unique candidates"
  putStrLn $ "  " ++ show numRedundant ++ "/" ++ show numCandidates ++ " redundant candidates"
  putStrLn ""
  printThy thy
  putStrLn $ unlines . map (showClass) $ filter (\xs -> length xs > 1) $ classes
  where
  numCandidates  =  length cs
  numUnique      =  length classes
  numRedundant   =  numCandidates - numUnique
  classes  =  classifyCandidates nm f cs
  cs       =  concat css
  css      =  take n css'
  (css', thy)    =  candidateDefnsC args nm f ps
  nRules  =  length (rules thy)
  nREs  =  length (equations thy) + nRules

-- shows a class of candidates
showClass :: [Defn] -> String
showClass cs  =  "class of " ++ show (length cs) ++ " equivalent candidates:\n\n"
              ++ unlines (map (indent . showDefn) cs)


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
