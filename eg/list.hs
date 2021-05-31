-- list.hs: conjuring functions over lists (of ints)
--
-- Copyright (C) 2021 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure
import Data.List (insert)

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

main :: IO ()
main = do
  -- length xs  =  if null xs then 0 else 1 + length (tail xs)
  --               1  2    3       4      5 6 7       8    9
  conjure "length" length'
    [ val (0 :: Int)
    , val (1 :: Int)
    , value "+" ((+) :: Int -> Int -> Int)
    , value "tail" (tail :: [Int] -> [Int])
    , value "null" (null :: [Int] -> Bool)
    ]

  -- reverse xs  =  if null xs then [] else reverse (tail xs) ++ [head xs]
  --                1  2    3       4       5        6    7   8  9 10 11 12
  -- needs size 11 with unit
  conjure "reverse" reverse'
    [ val ([] :: [Int])
    , value "unit" ((:[]) :: Int -> [Int])
    , value "++" ((++) :: [Int] -> [Int] -> [Int])
    , value "head" (head :: [Int] -> Int)
    , value "tail" (tail :: [Int] -> [Int])
    , value "null" (null :: [Int] -> Bool)
    ]

  -- sort xs  =  if null xs then [] else insert (head xs) (sort (tail xs))
  --             1  2    3       4       5       6    7    8     9    10
  conjure "sort" sort'
    [ val ([] :: [Int])
    , value "insert" (insert :: Int -> [Int] -> [Int])
    , value "head" (head :: [Int] -> Int)
    , value "tail" (tail :: [Int] -> [Int])
    , value "null" (null :: [Int] -> Bool)
    ]

  -- xs ++ ys  =  if null xs then ys else head xs:(tail xs ++ ys)
  --              1  2    3       4       5    6 7  8   9  10 11
  conjure "++" (+++)
    [ val ([] :: [Int])
    , value ":" ((:) :: Int -> [Int] -> [Int])
    , value "head" (head :: [Int] -> Int)
    , value "tail" (tail :: [Int] -> [Int])
    , value "null" (null :: [Int] -> Bool)
    ]

  -- now through fold
  -- length xs  =  foldr (const (1 +)) 0 xs
  conjure "length" length'
    [ val (0 :: Int)
    , val (1 :: Int)
    , value "+" ((+) :: Int -> Int -> Int)
    , value "foldr" (foldr :: (Int -> Int -> Int) -> Int -> [Int] -> Int)
    , value "const" (const :: (Int -> Int) -> Int -> (Int -> Int)) -- cheating?
    ]

  -- now through fold and some cheating
  --  reverse xs  =  foldr (\x xs -> xs ++ [x]) [] xs
  --  reverse xs  =  foldr (flip (++) . unit) [] xs
  conjure "reverse" reverse'
    [ val ([] :: [Int])
    , value "unit" ((:[]) :: Int -> [Int])
    , value "++" ((++) :: [Int] -> [Int] -> [Int])
    , value "foldr" (foldr :: (Int->[Int]->[Int]) -> [Int] -> [Int] -> [Int])
    -- these last two are cheats:
    , value "flip" (flip :: ([Int]->[Int]->[Int]) -> [Int] -> [Int] -> [Int])
    , value "." ((.) :: ([Int]->[Int]->[Int]) -> (Int->[Int]) -> Int -> [Int] -> [Int])
    ]

  -- now through fold
  -- sort xs  =  foldr insert [] xs
  conjure "sort" sort'
    [ val ([] :: [Int])
    , value "insert" (insert :: Int -> [Int] -> [Int])
    , value "foldr" (foldr :: (Int -> [Int] -> [Int]) -> [Int] -> [Int] -> [Int])
    ]

  -- now through fold
  -- xs ++ ys  =  foldr (:) ys xs
  conjure "++" (+++)
    [ val ([] :: [Int])
    , value ":" ((:) :: Int -> [Int] -> [Int])
    , value "foldr" (foldr :: (Int -> [Int] -> [Int]) -> [Int] -> [Int] -> [Int])
    ]

  -- intercalate
  -- xs \/ ys  =  if null xs then ys else head xs : (ys \/ tail xs)
  conjure "\\/" (\/)
    [ val ([] :: [Int])
    , value ":" ((:) :: Int -> [Int] -> [Int])
    , value "head" (head :: [Int] -> Int)
    , value "tail" (tail :: [Int] -> [Int])
    , value "null" (null :: [Int] -> Bool)
    ]
