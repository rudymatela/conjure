-- 2021, 2025  Colin Runciman
import Conjure
import Prelude hiding (sum, take, drop, zip)

sum :: [Int] -> Int
sum []       =  0
sum [0,1]    =  1
sum [1,0,1]  =  2

-- hoping for something like
-- sum []      =  0
-- sum (x:xs)  =  x + sum xs

sumBackground :: [Ingredient]
sumBackground =
  [ con (0::Int)
  , fun "+" ((+) :: Int -> Int -> Int)
  ]

app :: [Int] -> [Int] -> [Int]
app []      [0,1]  =  [0,1]
app [0,1]   []     =  [0,1]
app [0,1]   [0,1]  =  [0,1,0,1]

-- hoping for something like
-- app []     ys  =  ys
-- app (x:xs) ys  =  x : app xs ys

appBackground :: [Ingredient]
appBackground =
  [ fun ":" ((:) :: Int -> [Int] -> [Int])  
  ]

mem :: Int -> [Int] -> Bool
mem 0 []       =  False
mem 0 [0,1,1]  =  True
mem 0 [1,0,1]  =  True
mem 0 [1,1,0]  =  True
mem 0 [1,1,1]  =  False

-- hoping for something like
-- mem x []      =  False
-- mem x (y:ys)  =  x == y || mem x ys

memBackground :: [Ingredient]
memBackground =
  [ con False
  , fun "==" ((==) :: Int -> Int -> Bool)
  , fun "&&" ((&&) :: Bool -> Bool -> Bool)
  , fun "||" ((||) :: Bool -> Bool -> Bool)
  ]

sub :: [Int] -> [Int] -> Bool
sub []      []     =  True
sub []      [0,1]  =  True
sub [0]     [0,1]  =  True
sub [1]     [0,1]  =  True
sub [2]     [0,1]  =  False
sub [0,1]   [0,1]  =  True
sub [1,0]   [0,1]  =  False
sub [0,2]   [0,1]  =  False
sub [2,0]   [0,1]  =  False

-- Hoping for something like:
-- sub :: [Int] -> [Int] -> Bool
-- sub []     _       =  True
-- sub _      []      =  False
-- sub (x:xs) (y:ys)  =  if x == y then sub xs ys else sub (x:xs) ys

subBackground :: [Ingredient]
subBackground  =
  [ con True
  , con False
  , fun ":" ((:) :: Int -> [Int] -> [Int])
  , fun "==" ((==) :: Int -> Int -> Bool)
  , iif (undefined :: Bool)
  ]

set :: [Int] -> Bool
set []       =  True
set [0]      =  True
set [0,0]    =  False
set [0,1]    =  True
set [0,1,2]  =  True
set [0,0,1]  =  False
set [0,1,0]  =  False
set [0,1,1]  =  False

-- hoping for something like
-- set []      =  True
-- set (x:xs)  =  not (elem x xs) && set xs

setBackground :: [Ingredient]
setBackground =
  [ fun "not" (not :: Bool -> Bool)
  , fun "&&" ((&&) :: Bool -> Bool -> Bool)
  , fun "elem" (elem :: Int -> [Int] -> Bool)
  ]

take :: Int -> [A] -> [A]
take 0 []     =  []
take 1 []     =  []
take 0 [x,y]  =  []
take 1 [x,y]  =  [x]
take 2 [x,y]  =  [x,y]
take 3 [x,y]  =  [x,y]

-- hoping for something like
-- take 0 xs      =  []
-- take x []      =  []
-- take x (y:ys)  =  y : take (x-1) ys

takeBackground :: [Ingredient]
takeBackground  =
  [ con (0 :: Int)
  , con (1 :: Int)
  , con ([] :: [A])
  , fun "-" ((-) :: Int -> Int -> Int)
  , fun ":" ((:) :: A -> [A] -> [A])
  ]

drop :: Int -> [A] -> [A]
drop 0 []     =  []
drop 1 []     =  []
drop 0 [x,y]  =  [x,y]
drop 1 [x,y]  =  [y]
drop 2 [x,y]  =  []
drop 3 [x,y]  =  []

-- hoping for something like
-- drop 0 xs  =  xs
-- drop x []  =  []
-- drop x (y:ys)  =  drop (x-1) ys

dropBackground :: [Ingredient]
dropBackground  =
  [ con (0 :: Int)
  , con (1 :: Int)
  , con ([] :: [A])
  , fun "-" ((-) :: Int -> Int -> Int)
  ]

ord :: [Int] -> Bool
ord []       =  True
ord [0]      =  True
ord [0,1]    =  True
ord [1,0]    =  False
ord [0,1,0]  =  False
ord [0,1,1]  =  True

-- hoping for something like
-- ord []   =  True
-- ord [x]  =  True
-- ord (x:y:ys)  =  x <= y && ord (y:ys)

ordBackground :: [Ingredient]
ordBackground  =
  [ con True
  , fun "null" (null :: [Int] -> Bool)
  , fun "head" (head :: [Int] -> Int)
  , fun "<=" ((<=) :: Int -> Int -> Bool)
  , fun "&&" (&&)
  , fun "||" (||)
  ]

merge :: [Int] -> [Int] -> [Int]
merge []    [0,1]  =  [0,1]
merge [0,1] []     =  [0,1]
merge [0,2] [1,3]  =  [0,1,2,3]
merge [1,3] [0,2]  =  [0,1,2,3]

-- hoping for something like
merge [] ys  =  ys
merge xs []  =  xs
merge (x:xs) (y:ys)  =  if x <= y
                        then x : merge xs (y:ys)
                        else y : merge (x:xs) ys

mergeBackground :: [Ingredient]
mergeBackground  =
  [ fun "<=" ((<=) :: Int -> Int -> Bool)
  , con ([] :: [Int])
  , fun ":" ((:) :: Int -> [Int] -> [Int])
  , iif (undefined :: [Int])
  ]

zip :: [A] -> [B] -> [(A,B)]
zip []    []     =  []
zip []    [x,y]  =  []
zip [v,w] []     =  []
zip [v,w] [x,y]  =  [(v,x),(w,y)]

-- hoping for something like
-- zip []     _       =  []
-- zip (x:xs) []      =  []
-- zip (x:xs) (y:ys)  =  (x,y) : zip xs ys

zipBackground :: [Ingredient]
zipBackground  =
  [ con ([] :: [(A,B)])
  , fun "(,)" ((,) :: A -> B -> (A,B))
  , fun ":" ((:) :: (A,B) -> [(A,B)] -> [(A,B)])
  ]

assocs :: Int -> [(Int,A)] -> [A]
assocs 1 []             =  []
assocs 0 [(0,x),(1,y)]  =  [x]
assocs 1 [(0,x),(1,y)]  =  [y]
assocs 2 [(2,x),(2,y)]  =  [x,y]

-- hoping for something like
-- assocs _ []      =  []
-- assocs n (x:xs)  =  if fst x == n then snd x : assocs n xs else assocs n xs

assocsBackground :: [Ingredient]
assocsBackground  =
  [ con ([] :: [A])
  , fun "==" ((==) :: Int -> Int -> Bool)
  , fun ":" ((:) :: A -> [A] -> [A])
  , fun "fst" (fst :: (Int,A) -> Int)
  , fun "snd" (snd :: (Int,A) -> A)
  , iif (undefined :: [A])
  ]

main :: IO ()
main = do
  conjure "sum" sum sumBackground
  conjure "app" app appBackground
  conjure "mem" mem memBackground
  conjure "sub" sub subBackground
  conjure "set" set setBackground
  conjure "take" take takeBackground
  conjure "drop" drop dropBackground
  conjure "ord" ord ordBackground
  conjure "merge" merge mergeBackground
  conjure "zip" zip zipBackground
  conjure "assocs" assocs assocsBackground
