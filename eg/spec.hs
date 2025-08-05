-- spec.hs: conjuring a function from a specification
--
-- Adapted from Colin Runciman's example "ListFuns"
import Conjure
import Test.LeanCheck (holds, exists)
import Prelude hiding (sum)


squareSpec :: (Int -> Int) -> [Property]
squareSpec square  =
  [ property $ square 0 == 0
  , property $ square 1 == 1
  , property $ square 2 == 4
  ]

squareIngredients :: [Ingredient]
squareIngredients  =
  [ con (0::Int)
  , con (1::Int)
  , fun "+" ((+) :: Int -> Int -> Int)
  , fun "*" ((*) :: Int -> Int -> Int)
  ]

squarePropertySpec :: (Int -> Int) -> [Property]
squarePropertySpec square  =
  [ property $ \x -> square x >= x
  , property $ \x -> square x >= 0
  , property $ square 2 == 4
  ]


sumSpec :: ([Int] -> Int) -> [Property]
sumSpec sum  =
  [ property $ sum []      == 0
  , property $ sum [1,2]   == 3
  , property $ sum [3,4,5] == 12
  ]

sumIngredients :: [Ingredient]
sumIngredients  =
  [ fun "null" (null :: [Int] -> Bool)
  , con (0::Int)
  , fun "+"    ((+) :: Int -> Int -> Int)
  , fun "head" (head :: [Int] -> Int)
  , fun "tail" (tail :: [Int] -> [Int])
  ]


appSpec :: ([Int] -> [Int] -> [Int]) -> [Property]
appSpec (++)  =
  [ property $ []      ++ [0,1]   == [0,1]
  , property $ [2,3]   ++ []      == [2,3]
  , property $ [4,5,6] ++ [7,8,9] == [4,5,6,7,8,9]
  ]

appIngredients :: [Ingredient]
appIngredients =
  [ fun "null" (null :: [Int] -> Bool)
  , fun ":"    ((:) :: Int -> [Int] -> [Int])
  , fun "head" (head :: [Int] -> Int)
  , fun "tail" (tail :: [Int] -> [Int])
  ]


main :: IO ()
main = do
  conjureFromSpec "square" squareSpec squareIngredients
  conjureFromSpec "square" squarePropertySpec squareIngredients
  conjureFromSpec "sum" sumSpec sumIngredients
  conjureFromSpec "++"  appSpec appIngredients
