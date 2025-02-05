-- spec.hs: conjuring a function from a specification
--
-- Adapted from Colin Runciman's example "ListFuns"
import Conjure
import Test.LeanCheck (holds, exists)
import Prelude hiding (sum)


squareSpec :: (Int -> Int) -> Bool
squareSpec square  =  square 0 == 0
                   && square 1 == 1
                   && square 2 == 4

squarePrimitives :: [Prim]
squarePrimitives  =
  [ pr (0::Int)
  , pr (1::Int)
  , prim "+" ((+) :: Int -> Int -> Int)
  , prim "*" ((*) :: Int -> Int -> Int)
  ]

squarePropertySpec :: (Int -> Int) -> Bool
squarePropertySpec square  =  and
  [ holds n $ \x -> square x >= x
  , holds n $ \x -> square x >= 0
  , exists n $ \x -> square x > x
  ]  where  n = 60


sumSpec :: ([Int] -> Int) -> Bool
sumSpec sum  =  sum []      == 0
             && sum [1,2]   == 3
             && sum [3,4,5] == 12

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

appPrimitives :: [Prim]
appPrimitives =
  [ prim "null" (null :: [Int] -> Bool)
  , prim ":"    ((:) :: Int -> [Int] -> [Int])
  , prim "head" (head :: [Int] -> Int)
  , prim "tail" (tail :: [Int] -> [Int])
  ]


main :: IO ()
main = do
  conjureFromSpec "square" squareSpec squarePrimitives
  conjureFromSpec "square" squarePropertySpec squarePrimitives
  conjureFromSpec "sum" sumSpec sumPrimitives
  conjureFromSpec "++"  appSpec appPrimitives
