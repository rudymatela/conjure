-- higher.hs: conjuring higher order functions
--
-- Copyright (C) 2024 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure

appSpec :: ((Int -> Int) -> Int -> Int) -> Bool
appSpec ($)  =  ((+1) $ 2) == 3
             && (abs $ (-2)) == 2
             && (negate $ 1) == -1

composeSpec :: ((Int->Int) -> (Int->Int) -> Int->Int) -> Bool
composeSpec (.)  =  ((*2) . (*3)) 1 == 6
                 && (abs . negate) 7 == 7
                 && (negate . abs) 8 == -8

flipSpec :: ((Int -> Int -> Int) -> Int -> Int -> Int) -> Bool
flipSpec flip  =  flip const 1 2 == 2
               && flip div 2 10 == 5
               && flip mod 3 10 == 1

mapSpec :: ((Int -> Int) -> [Int] -> [Int]) -> Bool
mapSpec map  =  map (*2) [1,2,3] == [2,4,6]
             && map abs [0,-1,-2] == [0,1,2]

foldSpec :: ((Int -> Int -> Int) -> Int -> [Int] -> Int) -> Bool
foldSpec fold  =  fold (+) 0 [1,2,3] == 6
               && fold (*) 1 [1,2,3] == 6
               && fold (+) 0 [1,2,3,4] == 10
               && fold (*) 1 [1,2,3,4] == 24

filterSpec :: ((Int -> Bool) -> [Int] -> [Int]) -> Bool
filterSpec filter  =  filter (<0) [0,1,-1,2,-2] == [-1,-2]
                   && filter (>0) [0,1,-1,2,-2] == [1,2]
                   && filter odd [0,1,2,3,4,5,6] == [1,3,5]
                   && filter even [0,1,2,3,4,5,6] == [0,2,4,6]

main :: IO ()
main = do
  conjureFromSpec "$"      appSpec     primitives
  conjureFromSpec "."      composeSpec primitives
  conjureFromSpec "flip"   flipSpec    primitives
  conjureFromSpec "map"    mapSpec     primitives
  conjureFromSpec "fold"   foldSpec    primitives
  conjureFromSpec "filter" filterSpec  primitives


primitives :: [Prim]
primitives  =
  [ pr ([] :: [Int])
  , prim ":" ((:) :: Int -> [Int] -> [Int])
  , prif (undefined :: [Int])
  ]
