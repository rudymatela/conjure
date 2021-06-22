-- ill-hit.hs: failing to hit a function completely
--
-- Based on an example sent by Colin Runciman
--
-- Even though sum' is defined for 6 prims,
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
  -- the following does not hit all 6 defined prims, only 4
  conjure "sum" (sum' :: [Int] -> Int) primitives

  -- the following forces 3 argument prims, totaling 6
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

primitives :: [Prim]
primitives =
  [ pr (0 :: Int)
  , pr (1 :: Int)
  , prim "+" ((+) :: Int -> Int -> Int)
  , prim "*" ((*) :: Int -> Int -> Int)
  , prim "null" (null :: [Int] -> Bool)
  , prim "head" (head :: [Int] -> Int)
  , prim "tail" (tail :: [Int] -> [Int])
  ]
