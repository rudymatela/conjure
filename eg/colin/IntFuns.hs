-- 2021  Colin Runciman
-- Updated in 2024 to use the new Conjure interface
import Conjure
import Prelude hiding (exp)

tri :: Int -> Int
tri 1 = 1
tri 2 = 3
tri 3 = 6
tri 5 = 15

-- hoping for something like
-- tri x = if x==1 then 1 else x + tri (dec x)

triPrimitives :: [Prim]
triPrimitives =
  [ prim "==" ((==) :: Int -> Int -> Bool)
  , pr (1::Int)
  , prim "+" ((+) :: Int -> Int -> Int)
  , prim "dec" ((\x -> x-1) :: Int -> Int)
  ]

-- looking through 3039 candidates, 25% match, 1/4 assignments
-- tri x  =  if x == 1 then 1 else tri x

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib 2 = 2
fib 5 = 8
fib 6 = 13
fib 7 = 21

-- hoping for something like
-- fib x = if x <= 1 then 1 else fib (dec (dec x)) + fib (dec x)
--         1  2 3  4      5      6    7    8   9   10 11  12  13
fibPrimitives :: [Prim]
fibPrimitives =
  [ prim "<=" ((<=) :: Int -> Int -> Bool)
  , pr (0::Int)
  , pr (1::Int)
  , prim "+" ((+) :: Int -> Int -> Int)
  , prim "dec" ((\x -> x-1) :: Int -> Int)
  ]

-- looking through 23418 candidates
-- cannot conjure

exp :: Int -> Int
exp 1 = 1
exp 2 = 4
exp 3 = 27
exp 5 = 3125

-- hoping for
-- exp x = product (replicate x x)
expPrimitives :: [Prim]
expPrimitives =
  [ prim "replicate" (replicate :: Int -> Int -> [Int])
  , prim "product" (product :: [Int] -> Int)
  ]

-- looking through 18 candidates
-- cannot conjure

pri :: Int -> Bool
pri 1 = False
pri 2 = True
pri 3 = True
pri 4 = False
pri 5 = True
pri 6 = False
pri 7 = True

-- hoping for something like
-- pri p = factors p == list2 1 p
priPrimitives :: [Prim]
priPrimitives =
  [ prim "==" ((==) :: [Int] -> [Int] -> Bool)
  , prim "factors" ((\n -> filter (\k -> n `mod` k == 0) [1..n]) :: Int -> [Int])
  , prim "list2" ((\i j -> [i,j]) :: Int -> Int -> [Int])
  , pr (1 :: Int)
  ]

-- looking through 38 candidates, 100% match, 7/7 assignments
-- pri x  =  factors x == list2 1 x
-- BINGO!

main :: IO ()
main = do
  conjureWithMaxSize 10 "tri" tri triPrimitives
  conjureWith args{maxSize=13} "fib" fib fibPrimitives
  conjure "exp" exp expPrimitives
  conjure "pri" pri priPrimitives

