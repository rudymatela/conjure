-- ill-hit.hs: failing to hit a function completely
--
-- Based on an example sent by Colin Runciman
--
-- Even though sum' is defined with 6 equations,
-- Conjure only ever hits 4 of them.
import Conjure

sum' :: [Int] -> Int
sum' []  =  0
sum' [1]  =  1
sum' [1,2]  =  3
sum' [1,2,3]  =  6
sum' [3,4,5]  =  12
sum' [1,2,3,4]  =  10

sumSpec :: ([Int] -> Int) -> Bool
sumSpec sum  =  sum [] == 0
             && sum [1] == 1
             && sum [1,2] == 3
             && sum [1,2,3] == 6
             && sum [3,4,5] == 12
             && sum [1,2,3,4] == 10

main :: IO ()
main  =  do
  -- the following conjures from an "empty" spec
  -- we should get a proper error message
  conjure "sum" (undefined :: [Int] -> Int) ingredients

  -- the following does not hit all 6 defined prims, only 4
  conjure "sum" (sum' :: [Int] -> Int) ingredients

  -- the following conjures from a spec
  conjureFromSpec "sum" sumSpec ingredients

ingredients :: [Ingredient]
ingredients =
  [ unfun (0 :: Int)
  , unfun (1 :: Int)
  , fun "+" ((+) :: Int -> Int -> Int)
  , fun "*" ((*) :: Int -> Int -> Int)
  , fun "null" (null :: [Int] -> Bool)
  , fun "head" (head :: [Int] -> Int)
  , fun "tail" (tail :: [Int] -> [Int])
  ]
