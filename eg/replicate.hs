-- replicate.hs: replicate and other functions
--
-- Copyright (C) 2021-2025 Rudy Matela
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
    [ pr (0 :: Int)
    , pr (1 :: Int)
    , prim "-" ((-) :: Int -> Int -> Int)
    , pr ""
    , prim ":" ((:) :: Char -> String -> String)
    ]

  -- emulates how MagicHaskeller generates "replicates"
  conjure "replicates" replicates'
    [ prim "replicate" (replicate :: Int -> String -> [String])
    , prim "transpose" (transpose :: [[Char]] -> [[Char]])
    , prim "concat"    (concat :: [String] -> String)
    ]

  -- emulates an alternative generation that works on MagicHaskeller
  conjure "replicates" replicates'
    [ prim "replicate" (replicate :: Int -> Char -> String)
    , prim "map"       (map :: (Char -> String) -> String -> [String])
    , prim "concat"    (concat :: [String] -> String)
    ]

  -- alternative generation using recursion
  conjure "replicates" replicates'
    [ pr ""
    , prim ":" ((:) :: Char -> String -> String)
    , prim "++" ((++) :: String -> String -> String)
    , prim "replicate" (replicate :: Int -> Char -> String)
    ]

replicates n []  =  []
replicates n (x:xs)  =  replicate n x ++ replicates n xs
