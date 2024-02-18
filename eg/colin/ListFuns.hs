import Conjure
import Prelude hiding (sum, take, drop)

sum :: Spec1 [Int] Int
sum  =  [ []       -=  0
        , [0,1]    -=  1
        , [1,0,1]  -=  2 ]

-- hoping for something like
-- sum xs = if null xs then 0 else head xs + sum (tail xs)

sumBackground :: [Expr]
sumBackground =
  [ value "null" (null :: [Int] -> Bool)
  , val (0::Int)
  , value "+"    ((+) :: Int -> Int -> Int)
  , value "head" (head :: [Int] -> Int)
  , value "tail" (tail :: [Int] -> [Int])
  ]

app :: Spec2 [Int] [Int] [Int]
app  =  [ (,) []      [0,1]  -=  [0,1]
        , (,) [0,1]   []     -=  [0,1]
        , (,) [0,1]   [0,1]  -=  [0,1,0,1] ]

-- hoping for something like
-- app xs ys = if null xs then ys else head xs : app (tail xs) ys

appBackground :: [Expr]
appBackground =
  [ value "null" (null :: [Int] -> Bool)
  , value ":"    ((:) :: Int -> [Int] -> [Int])
  , value "head" (head :: [Int] -> Int)
  , value "tail" (tail :: [Int] -> [Int])
  ]

mem :: Spec2 Int [Int] Bool
mem  =  [ (,) 0 []       -=  False
        , (,) 0 [0,1,1]  -=  True
        , (,) 0 [1,0,1]  -=  True
        , (,) 0 [1,1,0]  -=  True
        , (,) 0 [1,1,1]  -=  False ]

-- hoping for something like
-- mem x xs = not (null xs) && (x == head xs || mem x (tail xs))

memBackground :: [Expr]
memBackground =
  [ value "null" (null :: [Int] -> Bool)
  , value "==" ((==) :: Int -> Int -> Bool)
  , value "not" (not :: Bool -> Bool)
  , value "&&" ((&&) :: Bool -> Bool -> Bool)
  , value "||" ((||) :: Bool -> Bool -> Bool)
  , value "head" (head :: [Int] -> Int)
  , value "tail" (tail :: [Int] -> [Int])
  ]

set :: Spec1 [Int] Bool
set  =  [ []     -=  True
        , [0]    -=  True
        , [0,0]  -=  False
        , [0,1]  -=  True
        , [0,1,2] -= True
        , [0,0,1] -= False
        , [0,1,0] -= False
        , [0,1,1] -= False ]

-- hoping for something like
-- set xs = null xs || not (elem (head xs) (tail xs)) && set (tail xs)

setBackground :: [Expr]
setBackground =
  [ value "null" (null :: [Int] -> Bool)
  , value "not" (not :: Bool -> Bool)
  , value "&&" ((&&) :: Bool -> Bool -> Bool)
  , value "||" ((||) :: Bool -> Bool -> Bool)
  , value "head" (head :: [Int] -> Int)
  , value "tail" (tail :: [Int] -> [Int])
  , value "elem" (elem :: Int -> [Int] -> Bool)
  ]

take :: Spec2 Int [Int] [Int]
take  =  [ (,) 0 []     -=  []
         , (,) 1 []     -=  []
         , (,) 0 [0,1]  -=  []
         , (,) 1 [0,1]  -=  [0]
         , (,) 2 [0,1]  -=  [0,1]
         , (,) 3 [0,1]  -=  [0,1] ]

-- hoping for something like
-- take n xs = if n==0 || null xs then [] else head xs : take (dec n) (tail xs)

takeBackground :: [Expr]
takeBackground =
  [ val (0 :: Int)
  , val ([] :: [Int])
  , value "null" (null :: [Int] -> Bool)
  , value "==" ((==) :: Int -> Int -> Bool)
  , value "||" ((||) :: Bool -> Bool -> Bool)
  , value "dec" ((\n -> n-1) :: Int -> Int)
  , value ":" ((:) :: Int -> [Int] -> [Int])
  , value "head" (head :: [Int] -> Int)
  , value "tail" (tail :: [Int] -> [Int])
  ]

drop :: Spec2 Int [Int] [Int]
drop  =  [ (,) 0 []     -=  []
         , (,) 1 []     -=  []
         , (,) 0 [0,1]  -=  [0,1]
         , (,) 1 [0,1]  -=  [1]
         , (,) 2 [0,1]  -=  []
         , (,) 3 [0,1]  -=  [] ]

-- hoping for something like
-- drop n xs = if n==0 || null xs then xs else drop (dec n) (tail xs)

dropBackground :: [Expr]
dropBackground =
  [ val (0 :: Int)
  , value "null" (null :: [Int] -> Bool)
  , value "==" ((==) :: Int -> Int -> Bool)
  , value "||" ((||) :: Bool -> Bool -> Bool)
  , value "dec" ((\n -> n-1) :: Int -> Int)
  , value "tail" (tail :: [Int] -> [Int])
  ]

main :: IO ()
main = do
  conjure1With args{maxSize=15} "sum" sum sumBackground
  conjure2With args{maxSize=15} "app" app appBackground
  conjure2With args{maxSize=15} "mem" mem memBackground
  conjure1With args{maxSize=15} "set" set setBackground
  conjure2With args{maxSize=20} "take" take takeBackground
  conjure2With args{maxSize=15} "drop" drop dropBackground

