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
    , prim "unit" ((:[]) :: Int -> [Int])
    , prim "++" ((++) :: [Int] -> [Int] -> [Int])
    ]

  conjure "++" (+++)
    [ pr ([] :: [Int])
    , prim ":" ((:) :: Int -> [Int] -> [Int])
    ]

  -- now through fold
  -- length xs  =  foldr (const (1 +)) 0 xs
  conjure "length" length'
    [ pr (0 :: Int)
    , pr (1 :: Int)
    , prim "+" ((+) :: Int -> Int -> Int)
    , prim "foldr" (foldr :: (Int -> Int -> Int) -> Int -> [Int] -> Int)
    , prim "const" (const :: (Int -> Int) -> Int -> (Int -> Int)) -- cheating?
    ]

  -- now through fold and some cheating
  --  reverse xs  =  foldr (\x xs -> xs ++ [x]) [] xs
  --  reverse xs  =  foldr (flip (++) . unit) [] xs
  conjure "reverse" reverse'
    [ pr ([] :: [Int])
    , prim "unit" ((:[]) :: Int -> [Int])
    , prim "++" ((++) :: [Int] -> [Int] -> [Int])
    , prim "foldr" (foldr :: (Int->[Int]->[Int]) -> [Int] -> [Int] -> [Int])
    -- these last two are cheats:
    , prim "flip" (flip :: ([Int]->[Int]->[Int]) -> [Int] -> [Int] -> [Int])
    , prim "." ((.) :: ([Int]->[Int]->[Int]) -> (Int->[Int]) -> Int -> [Int] -> [Int])
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
