-- higher.hs: conjuring higher order functions
--
-- Copyright (C) 2024-2025 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure

appSpec :: ((Int -> Int) -> Int -> Int) -> [Property]
appSpec ($)  =
  [ property (((+1) $ 2) == 3)
  , property ((abs $ (-2)) == 2)
  , property ((negate $ 1) == -1)
  ]

composeSpec :: ((Int->Int) -> (Int->Int) -> Int->Int) -> [Property]
composeSpec (.)  =
  [ property $ ((*2) . (*3)) 1 == 6
  , property $ (abs . negate) 7 == 7
  , property $ (negate . abs) 8 == -8
  ]

flipSpec :: ((Int -> Int -> Int) -> Int -> Int -> Int) -> [Property]
flipSpec flip  =
  [ property $ flip const 1 2 == 2
  , property $ flip div 2 10 == 5
  , property $ flip mod 3 10 == 1
  ]

mapSpec :: ((Int -> Int) -> [Int] -> [Int]) -> [Property]
mapSpec map  =
  [ property $ map (*2) [1,2,3] == [2,4,6]
  , property $ map abs [0,-1,-2] == [0,1,2]
  ]

foldSpec :: ((Int -> Int -> Int) -> Int -> [Int] -> Int) -> [Property]
foldSpec fold  =
  [ property $ fold (+) 0 [1,2,3] == 6
  , property $ fold (*) 1 [1,2,3] == 6
  , property $ fold (+) 0 [1,2,3,4] == 10
  , property $ fold (*) 1 [1,2,3,4] == 24
  ]

filterSpec :: ((Int -> Bool) -> [Int] -> [Int]) -> [Property]
filterSpec filter  =
  [ property $ filter (<0) [0,1,-1,2,-2] == [-1,-2]
  , property $ filter (>0) [0,1,-1,2,-2] == [1,2]
  , property $ filter odd [0,1,2,3,4,5,6] == [1,3,5]
  , property $ filter even [0,1,2,3,4,5,6] == [0,2,4,6]
  ]

main :: IO ()
main = do
  conjureFromSpec "$"      appSpec     ingredients
  conjureFromSpec "."      composeSpec ingredients
  conjureFromSpec "flip"   flipSpec    ingredients
  conjureFromSpec "map"    mapSpec     ingredients
  conjureFromSpec "fold"   foldSpec    ingredients
  conjureFromSpec "filter" filterSpec  ingredients


ingredients :: [Ingredient]
ingredients  =
  [ unfun ([] :: [Int])
  , fun ":" ((:) :: Int -> [Int] -> [Int])
  , guard
  ]
