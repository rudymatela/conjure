-- setelem.hs: elem and set functions
--
-- Copyright (C) 2021-2025 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure

elem' :: Int -> [Int] -> Bool
elem' 0 [1]  =  False
elem' 1 [1,2]  =  True
elem' 0 [1,2]  =  False
elem' 2 [0,1,2]  =  True

set' :: [Int] -> Bool
set' [1,1]  =  False
set' [1,2]  =  True
set' [1,0,0]  =  False
set' [1,2,3]  =  True

main :: IO ()
main = do
  conjure "elem" (elem')
    [ con True
    , con False
    , fun "||" (||)
    , fun "&&" (&&)
    , fun "not" not
    , con ([] :: [Int])
    , fun ":" ((:) :: Int -> [Int] -> [Int])
    , fun "==" ((==) :: Int -> Int -> Bool)
    ]

  conjure "set" (set')
    [ con True
    , con False
    , fun "||" (||)
    , fun "&&" (&&)
    , fun "not" not
    , con ([] :: [Int])
    , fun ":" ((:) :: Int -> [Int] -> [Int])
    , fun "elem" (elem :: Int -> [Int] -> Bool)
    ]
