-- list.hs: conjuring functions over lists (of ints)
--
-- Copyright (C) 2021 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure

length' :: [Int] -> Int
length' []       =  0
length' [x]      =  1
length' [x,y]    =  2
length' [x,y,z]  =  3

reverse' :: [Int] -> [Int]
reverse' []       =  []
reverse' [x]      =  [x]
reverse' [x,y]    =  [y,x]
reverse' [x,y,z]  =  [z,y,x]

sort' :: [Int] -> [Int]
sort' []       =  []
sort' [x]      =  [x]
sort' [x,y]
  | x <= y     =  [x,y]
  | otherwise  =  [y,x]
sort' [x,y,z]
  | x <= y && y <= z  =  [x,y,z]
  | z <= y && y <= x  =  [z,y,x]

(+++) :: [Int] -> [Int] -> [Int]
[x]     +++ [y]      =  [x,y]
[x,y]   +++ [z,w]    =  [x,y,z,w]
[x,y,z] +++ [w,v,u]  =  [x,y,z,w,v,u]

(\/) :: [Int] -> [Int] -> [Int]
[x] \/ [y]  =  [x,y]
[x,y] \/ [z,w]  =  [x,z,y,w]
[x,y,z] \/ [w,v,u]  =  [x,w,y,v,z,u]

merge' :: [Int] -> [Int] -> [Int]
merge' [] []  =  []
merge' xs []  =  xs
merge' [] ys  =  ys
merge' [x] [y] | x <= y     =  [x,y]
               | otherwise  =  [y,x]
merge' [0,1] [0,1]  =  [0,0,1,1]
merge' [0,1] [2,3]  =  [0,1,2,3]
merge' [0,2] [1,3]  =  [0,1,2,3]

ordered' :: [Int] -> Bool
ordered' []  =  True
ordered' [x]  =  True
ordered' [x,y]  =  x <= y
ordered' [x,y,z]  =  x <= y && y <= z

main :: IO ()
main = do
  conjure "length" length'
    [ pr (0 :: Int)
    , pr (1 :: Int)
    , prim "+" ((+) :: Int -> Int -> Int)
    ]

  conjure "reverse" reverse'
    [ pr ([] :: [Int])
    , prim ":" ((:) :: Int -> [Int] -> [Int])
    , prim "++" ((++) :: [Int] -> [Int] -> [Int])
    ]

  conjure "++" (+++)
    [ pr ([] :: [Int])
    , prim ":" ((:) :: Int -> [Int] -> [Int])
    ]

  conjure "++" (+++)
    [ pr ([] :: [Int])
    , prim ":" ((:) :: Int -> [Int] -> [Int])
    , prim "foldr" (foldr :: (Int -> [Int] -> [Int]) -> [Int] -> [Int] -> [Int])
    ]

  conjure "\\/" (\/)
    [ pr ([] :: [Int])
    , prim ":" ((:) :: Int -> [Int] -> [Int])
    ]

  conjure "ordered" ordered'
    [ pr False
    , pr True
    , prim "&&" (&&)
    , prim "||" (||)
    , prim "<=" ((<=) :: Int -> Int -> Bool)
    , prim "null" (null :: [Int] -> Bool)
    , prim "head" (head :: [Int] -> Int)
    , prim "tail" (tail :: [Int] -> [Int])
    ]

  -- unreachable: needs about 26, but can only reach 16
  conjureWithMaxSize 12 "merge" merge'
    [ prim ":" ((:) :: Int -> [Int] -> [Int])
    , prim "compare" (compare :: Int -> Int -> Ordering)
    , primOrdCaseFor (undefined :: [Int])
    ]
