-- spec.hs: conjuring a function from a specification
--
-- Adapted from Colin Runciman's example "ListFuns"
import Conjure
import Prelude hiding (sum)


sumSpec :: Spec1 [Int] Int
sumSpec  =
  [ []      -= 0
  , [1,2]   -= 3
  , [3,4,5] -= 12
  ]

-- hoping for something like
-- sum xs  =  if null xs then 0 else head xs + sum (tail xs)

sumPrimitives :: [Expr]
sumPrimitives  =
  [ value "null" (null :: [Int] -> Bool)
  , val (0::Int)
  , value "+"    ((+) :: Int -> Int -> Int)
  , value "head" (head :: [Int] -> Int)
  , value "tail" (tail :: [Int] -> [Int])
  ]


appSpec :: Spec2 [Int] [Int] [Int]
appSpec  =
  [ (,) []      [0,1]   -= [0,1]
  , (,) [2,3]   []      -= [2,3]
  , (,) [4,5,6] [7,8,9] -= [4,5,6,7,8,9]
  ]

-- hoping for something like
-- app xs ys = if null xs then ys else head xs : app (tail xs) ys

appPrimitives :: [Expr]
appPrimitives =
  [ value "null" (null :: [Int] -> Bool)
  , value ":"    ((:) :: Int -> [Int] -> [Int])
  , value "head" (head :: [Int] -> Int)
  , value "tail" (tail :: [Int] -> [Int])
  ]


main :: IO ()
main = do
  conjure1 "sum" sumSpec sumPrimitives
  conjure2 "++"  appSpec appPrimitives
