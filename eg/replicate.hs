-- replicate.hs: replicate and other functions
--
-- Copyright (C) 2021 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure
import Data.List (transpose)

replicate' :: Int -> Char -> String
replicate' 0 c  =  []
replicate' 1 c  =  [c]
replicate' 2 c  =  [c,c]
replicate' 3 c  =  [c,c,c]
replicate' 4 c  =  [c,c,c,c]

-- this function is one of the examples of MagicHaskeller
replicates' :: String -> Int -> String
replicates' [a]     1  =  [a]
replicates' [a,b]   1  =  [a,b]
replicates' [a]     2  =  [a,a]
replicates' [a,b]   2  =  [a,a,b,b]
replicates' [a,b,c] 2  =  [a,a,b,b,c,c]
replicates' [a]     3  =  [a,a,a]
replicates' [a,b]   3  =  [a,a,a,b,b,b]
replicates' [a,b,c] 3  =  [a,a,a,b,b,b,c,c,c]

main :: IO ()
main = do
  conjure "replicate" replicate'
    [ val (0 :: Int)
    , value "dec" (subtract 1 :: Int -> Int)
    , value "==" ((==) :: Int -> Int -> Bool)
    , val ""
    , value ":" ((:) :: Char -> String -> String)
    ]

  -- emulates how MagicHaskeller generates "replicates"
  conjureWith args{maxTests=360} "replicates" replicates'
    [ value "replicate" (replicate :: Int -> String -> [String])
    , value "transpose" (transpose :: [[Char]] -> [[Char]])
    , value "concat"    (concat :: [String] -> String)
    ]

  -- alternative generation using recursion
  conjureWith args{maxTests=360, maxSize=13} "replicates" replicates'
    [ val ""
    , value "null" (null :: String -> Bool)
    , value "head" (head :: String -> Char)
    , value "tail" (tail :: String -> String)
    , value ":" ((:) :: Char -> String -> String)
    , value "++" ((++) :: String -> String -> String)
    , value "replicate" (replicate :: Int -> Char -> String)
    ]

replicates n []  =  []
replicates n (x:xs)  =  replicate n x ++ replicates n xs
