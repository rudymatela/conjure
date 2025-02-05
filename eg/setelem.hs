-- setelem.hs: elem and set functions
--
-- Copyright (C) 2021-2025 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
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
  conjure "elem" (elem')
    [ pr ([] :: [Int])
    , pr True
    , pr False
    , prim "||" (||)
    , prim "&&" (&&)
    , prim "not" not
    , prim ":" ((:) :: Int -> [Int] -> [Int])
    , prim "==" ((==) :: Int -> Int -> Bool)
    ]

  conjure "set" (set')
    [ pr ([] :: [Int])
    , pr True
    , pr False
    , prim "&&" (&&)
    , prim "||" (||)
    , prim "not" not
    , prim ":" ((:) :: Int -> [Int] -> [Int])
    , prim "elem" (elem :: Int -> [Int] -> Bool)
    ]
