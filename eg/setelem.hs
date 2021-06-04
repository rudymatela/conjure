-- setelem.hs: elem and set functions
import Conjure

elem' :: Int -> [Int] -> Bool
elem' x [y]  =  x == y
elem' x [y,z]  =  x == y || x == z
elem' x [y,z,w]  =  x == y || x == z || x == w

set' :: [Int] -> Bool
set' [x]  =  True
set' [x,y]  =  not (x == y)
set' [x,y,z]  =  not (x == y || y == z || x == z)

main :: IO ()
main = do
  -- elem x xs  =  if null xs then False else elem x (tail xs) || x == head xs
  conjureWithMaxSize 13 "elem" (elem')
    [ val ([] :: [Int])
    , val True
    , val False
    , value "||" (||)
    , value "&&" (&&)
    , value ":" ((:) :: Int -> [Int] -> [Int])
    , value "head" (head :: [Int] -> Int)
    , value "tail" (tail :: [Int] -> [Int])
    , value "null" (null :: [Int] -> Bool)
    , value "==" ((==) :: Int -> Int -> Bool)
    ]

  -- set xs  =  if null xs then True else not (head xs `elem` tail xs) && set (tail xs)
  conjureWithMaxSize 14 "set" (set')
    [ val ([] :: [Int])
    , val True
    , val False
    , value "||" (||)
    , value "&&" (&&)
    , value ":" ((:) :: Int -> [Int] -> [Int])
    , value "head" (head :: [Int] -> Int)
    , value "tail" (tail :: [Int] -> [Int])
    , value "null" (null :: [Int] -> Bool)
    , value "elem" (elem :: Int -> [Int] -> Bool)
    ]
