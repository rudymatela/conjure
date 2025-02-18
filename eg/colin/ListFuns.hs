--- 2021, 2025  Colin Runciman
import Conjure
import Prelude hiding (sum, take, drop)

sum :: [Int] -> Int
sum []       =  0
sum [0,1]    =  1
sum [1,0,1]  =  2

-- hoping for something like
-- sum []      =  0
-- sum (x:xs)  =  x + sum xs

sumBackground :: [Prim]
sumBackground =
  [ pr (0::Int)
  , prim "+" ((+) :: Int -> Int -> Int)
  ]

app :: [Int] -> [Int] -> [Int]
app []      [0,1]  =  [0,1]
app [0,1]   []     =  [0,1]
app [0,1]   [0,1]  =  [0,1,0,1]

-- hoping for something like
-- app []     ys  =  ys
-- app (x:xs) ys  =  x : app xs ys

appBackground :: [Prim]
appBackground =
  [ prim ":" ((:) :: Int -> [Int] -> [Int])  
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

memBackground :: [Prim]
memBackground =
  [ pr False
  , prim "==" ((==) :: Int -> Int -> Bool)
  , prim "&&" ((&&) :: Bool -> Bool -> Bool)
  , prim "||" ((||) :: Bool -> Bool -> Bool)
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
-- sub (x:xs) (y:ys)  =  x == y && sub xs ys || sub (x:xs) ys

subBackground :: [Prim]
subBackground  =
  [ pr True
  , pr False
  , prim ":" ((:) :: Int -> [Int] -> [Int])
  , prim "==" ((==) :: Int -> Int -> Bool)
  , prim "&&" ((&&) :: Bool -> Bool -> Bool)
  , prim "||" ((||) :: Bool -> Bool -> Bool)
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

setBackground :: [Prim]
setBackground =
  [ prim "not" (not :: Bool -> Bool)
  , prim "&&" ((&&) :: Bool -> Bool -> Bool)
  , prim "elem" (elem :: Int -> [Int] -> Bool)
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

takeBackground :: [Prim]
takeBackground =
  [ pr (0 :: Int)
  , pr (1 :: Int)
  , pr ([] :: [A])
  , prim "-" ((-) :: Int -> Int -> Int)
  , prim ":" ((:) :: A -> [A] -> [A])
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

dropBackground :: [Prim]
dropBackground =
  [ pr (0 :: Int)
  , pr (1 :: Int)
  , pr ([] :: [A])
  , prim "-" ((-) :: Int -> Int -> Int)
  ]

main :: IO ()
main = do
  conjure "sum" sum sumBackground
  conjure "app" app appBackground
  conjure "mem" mem memBackground
  conjureWith args{maxSize=12,showCandidates=True} "sub" sub subBackground
  conjure "set" set setBackground
  conjure "take" take takeBackground
  conjure "drop" drop dropBackground

