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
  conjureWith args{requireZero=False} "tri" tri
    [ pr (1::Int)
    , prim "+" ((+) :: Int -> Int -> Int)
    , prim "-" ((-) :: Int -> Int -> Int)
    ]
