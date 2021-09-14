-- spec.hs: conjuring a function from a specification
--
-- Adapted from Colin Runciman's example "ListFuns"
import Conjure
import Prelude hiding (sum)


sumSpec :: ([Int] -> Int) -> Bool
sumSpec sum  =  sum []      == 0
             && sum [1,2]   == 3
             && sum [3,4,5] == 12

-- hoping for something like
-- sum xs  =  if null xs then 0 else head xs + sum (tail xs)

sumPrimitives :: [Prim]
sumPrimitives  =
  [ prim "null" (null :: [Int] -> Bool)
  , pr (0::Int)
  , prim "+"    ((+) :: Int -> Int -> Int)
  , prim "head" (head :: [Int] -> Int)
  , prim "tail" (tail :: [Int] -> [Int])
  ]


appSpec :: ([Int] -> [Int] -> [Int]) -> Bool
appSpec (++)  =  []      ++ [0,1]   == [0,1]
              && [2,3]   ++ []      == [2,3]
              && [4,5,6] ++ [7,8,9] == [4,5,6,7,8,9]

-- hoping for something like
-- app xs ys = if null xs then ys else head xs : app (tail xs) ys

appPrimitives :: [Prim]
appPrimitives =
  [ prim "null" (null :: [Int] -> Bool)
  , prim ":"    ((:) :: Int -> [Int] -> [Int])
  , prim "head" (head :: [Int] -> Int)
  , prim "tail" (tail :: [Int] -> [Int])
  ]


main :: IO ()
main = do
  conjureFromSpec "sum" sumSpec sumPrimitives
  conjureFromSpec "++"  appSpec appPrimitives
