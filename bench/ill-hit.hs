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

sumSpec :: [([Int],Int)]
sumSpec  =  [ [] -= 0
            , [1] -= 1
            , [1,2] -= 3
            , [1,2,3] -= 6
            , [3,4,5] -= 12
            , [1,2,3,4] -= 10
            ]

main :: IO ()
main  =  do
  -- the following does not hit all 6 defined values, only 4
  conjure "sum" (sum' :: [Int] -> Int) primitives

  -- the following forces 3 argument values, totaling 6
  conjureWith as "sum" (sum' :: [Int] -> Int) primitives

  -- the following conjures from a spec
  conjure1 "sum" sumSpec primitives

as :: Args
as  =  args
    {  forceTests = map (map val)
                  [ [[1,2]]
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
