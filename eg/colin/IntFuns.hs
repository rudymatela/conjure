import Conjure
import Prelude hiding (exp)

tri :: Int -> Int
tri 1 = 1
tri 2 = 3
tri 3 = 6
tri 5 = 15

-- hoping for something like
-- tri x = if x==1 then 1 else x + tri (dec x)

triBackground :: [Expr]
triBackground =
  [ value "==" ((==) :: Int -> Int -> Bool)
  , val (1::Int)
  , value "+" ((+) :: Int -> Int -> Int)
  , value "dec" ((\x -> x-1) :: Int -> Int)
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
fibBackground :: [Expr]
fibBackground =
  [ value "<=" ((<=) :: Int -> Int -> Bool)
  , val (1::Int)
  , value "+" ((+) :: Int -> Int -> Int)
  , value "dec" ((\x -> x-1) :: Int -> Int)
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
expBackground :: [Expr]
expBackground =
  [ value "replicate" (replicate :: Int -> Int -> [Int])
  , value "product" (product :: [Int] -> Int)
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
priBackground :: [Expr]
priBackground =
  [ value "==" ((==) :: [Int] -> [Int] -> Bool)
  , value "factors" ((\n -> filter (\k -> n `mod` k == 0) [1..n]) :: Int -> [Int])
  , value "list2" ((\i j -> [i,j]) :: Int -> Int -> [Int])
  , val (1 :: Int)
  ]

-- looking through 38 candidates, 100% match, 7/7 assignments
-- pri x  =  factors x == list2 1 x
-- BINGO!

main :: IO ()
main = do
  conjureWithMaxSize 10 "tri" tri triBackground
  conjureWith args{maxSize=13, maxRecursiveCalls=2, maxRecursionSize=360} "fib" fib fibBackground
  conjure "exp" exp expBackground
  conjure "pri" pri priBackground

