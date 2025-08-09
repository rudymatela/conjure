-- list.hs: conjuring functions over lists (of ints)
--
-- Copyright (C) 2021-2025 Rudy Matela
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
[1]     +++ [2]      =  [1,2]
[1,2]   +++ [0,1]    =  [1,2,0,1]
[0,1,0] +++ [1,0,1]  =  [0,1,0,1,0,1]

(\/) :: [Int] -> [Int] -> [Int]
[x] \/ [y]  =  [x,y]
[x,y] \/ [z,w]  =  [x,z,y,w]
[x,y,z] \/ [w,v,u]  =  [x,w,y,v,z,u]

ordered' :: [Int] -> Bool
ordered' []  =  True
ordered' [x]  =  True
ordered' [x,y]  =  x <= y
ordered' [x,y,z]  =  x <= y && y <= z

zip' :: [Int] -> [Int] -> [(Int,Int)]
zip' [] []  =  []
zip' [x] [a]  =  [(x,a)]
zip' [x,y] [a,b]  =  [(x,a),(y,b)]
zip' [x,y,z] [a,b,c]  =  [(x,a),(y,b),(z,c)]

last' :: [Int] -> Int
last' [x]  =  x
last' [x,y]  =  y
last' [x,y,z]  =  z

main :: IO ()
main = do
  conjure "length" length'
    [ con (0 :: Int)
    , con (1 :: Int)
    , fun "+" ((+) :: Int -> Int -> Int)
    ]

  conjure "reverse" reverse'
    [ con ([] :: [Int])
    , fun ":" ((:) :: Int -> [Int] -> [Int])
    , fun "++" ((++) :: [Int] -> [Int] -> [Int])
    ]

  conjure "++" (+++)
    [ con ([] :: [Int])
    , fun ":" ((:) :: Int -> [Int] -> [Int])
    ]

  conjure "++" (+++)
    [ con ([] :: [Int])
    , fun ":" ((:) :: Int -> [Int] -> [Int])
    , fun "foldr" (foldr :: (Int -> [Int] -> [Int]) -> [Int] -> [Int] -> [Int])
    ]

  conjure "last" last'
    [ con ([] :: [Int])
    , fun ":" ((:) :: Int -> [Int] -> [Int])
    , fun "null" (null :: [Int] -> Bool)
    , guard
    ]

  conjure "last" last'
    [ con ([] :: [Int])
    , fun ":" ((:) :: Int -> [Int] -> [Int])
    , maxPatternDepth 2
    ]

  conjure "zip" (zip')
    [ con ([] :: [(Int,Int)])
    , fun ":" ((:) :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)])
    , fun "," ((,) :: Int -> Int -> (Int,Int))
    ]

  conjure "\\/" (Main.\/)
    [ con ([] :: [Int])
    , fun ":" ((:) :: Int -> [Int] -> [Int])
    ]

  conjure "ordered" ordered'
    [ con False
    , con True
    , fun "&&" (&&)
    , fun "||" (||)
    , fun "<=" ((<=) :: Int -> Int -> Bool)
    , fun "null" (null :: [Int] -> Bool)
    , fun "head" (head :: [Int] -> Int)
    , fun "tail" (tail :: [Int] -> [Int])
    ]

  -- for elem, please see eg/setelem.hs
  -- for take and drop, please see eg/take-drop.hs
