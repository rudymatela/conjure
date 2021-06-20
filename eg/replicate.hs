-- replicate.hs: replicate and other functions
--
-- Copyright (C) 2021 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure
import Data.List (insert)

replicate' :: Int -> Char -> String
replicate' 0 c  =  []
replicate' 1 c  =  [c]
replicate' 2 c  =  [c,c]
replicate' 3 c  =  [c,c,c]
replicate' 4 c  =  [c,c,c,c]

main :: IO ()
main = do
  conjure "replicate" replicate'
    [ val (0 :: Int)
    , value "dec" (subtract 1 :: Int -> Int)
    , value "==" ((==) :: Int -> Int -> Bool)
    , val ""
    , value ":" ((:) :: Char -> String -> String)
    ]
