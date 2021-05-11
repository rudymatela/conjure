-- ill-hit.hs: failing to hit a function completely
--
-- Based on an example sent by Colin Runciman
--
-- Even though sum' is defined for 6 values,
-- Conjure only ever hits 4 of them.
import Conjure

sum' :: [Int] -> Int
sum' []  =  0
sum' [1]  =  1
sum' [1,2]  =  3
sum' [1,2,3]  =  6
sum' [3,4,5]  =  12
sum' [1,2,3,4]  =  10

main :: IO ()
main  =  do
  conjure "sum" (sum' :: [Int] -> Int) primitives
  conjureWith as "sum" (sum' :: [Int] -> Int) primitives

as :: Args
as  =  args
    {  forceTests = map (map val)
                  [ [[1,2::Int]]
                  , [[3,4,5::Int]]
                  , [[1,2,3,4]]
                  ]
    }

primitives :: [Expr]
primitives =
  [ val (0 :: Int)
  , val (1 :: Int)
  , value "+" ((+) :: Int -> Int -> Int)
  , value "*" ((*) :: Int -> Int -> Int)
  , value "null" (null :: [Int] -> Bool)
  , value "head" (head :: [Int] -> Int)
  , value "tail" (tail :: [Int] -> [Int])
  ]
