-- setelem.hs: elem and set functions
import Conjure

elem' :: Int -> [Int] -> Bool
elem' x [y]  =  x == y
elem' x [y,z]  =  x == y || x == z
elem' x [y,z,w]  =  x == y || x == z || x == w

set' :: [Int] -> Bool
set' []  =  True
set' [x]  =  True
set' [x,y]  =  not (x == y)
set' [x,y,z]  =  not (x == y || y == z || x == z)

main :: IO ()
main = do
  -- elem x xs  =  not (null xs) && (elem x (tail xs) || x == head xs)
  --               1    2    3    4  5    6   7   8   9  10 11 12  13
  conjureWithMaxSize 13 "elem" (elem')
    [ pr ([] :: [Int])
    , pr True
    , pr False
    , prim "||" (||)
    , prim "&&" (&&)
    , prim "not" not
    , prim ":" ((:) :: Int -> [Int] -> [Int])
    , prim "head" (head :: [Int] -> Int)
    , prim "tail" (tail :: [Int] -> [Int])
    , prim "null" (null :: [Int] -> Bool)
    , prim "==" ((==) :: Int -> Int -> Bool)
    ]

  -- set xs  =  null xs || set (tail xs) && not (elem (head xs) (tail xs))
  --            1    2  3  4    5    6    7  8    9    10   11   12   13
  conjureWithMaxSize 13 "set" (set')
    [ pr ([] :: [Int])
    , pr True
    , pr False
    , prim "&&" (&&)
    , prim "||" (||)
    , prim "not" not
    , prim ":" ((:) :: Int -> [Int] -> [Int])
    , prim "head" (head :: [Int] -> Int)
    , prim "tail" (tail :: [Int] -> [Int])
    , prim "null" (null :: [Int] -> Bool)
    , prim "elem" (elem :: Int -> [Int] -> Bool)
    ]
