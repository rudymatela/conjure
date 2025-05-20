-- tri.hs: conjuring a tri function
--
-- 2025 Colin Runciman and Rudy Matela
import Conjure

tri :: Int -> Int
tri 1  =  1
tri 2  =  3
tri 3  =  6
tri 5  =  15

main :: IO ()
main  =  do
  conjure "tri" tri
    [ unfun (1::Int)
    , fun "+" ((+) :: Int -> Int -> Int)
    , fun "-" ((-) :: Int -> Int -> Int)
    ]
