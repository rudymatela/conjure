-- fib01.hs: conjuring an efficient fibonacci function
import Conjure

fib01 :: Int -> Int -> Int -> Int
fib01 0 1 0  =  1
fib01 0 1 1  =  1
fib01 0 1 2  =  2
fib01 0 1 3  =  3
fib01 0 1 4  =  5
fib01 0 1 5  =  8
fib01 0 1 6  =  13
fib01 0 1 7  =  21

main :: IO ()
main  =  do
  conjureWithMaxSize 5 "fib01" fib01
    [ pr (0::Int)
    , prim "dec" (subtract 1 :: Int -> Int)
    , prim "+" ((+) :: Int -> Int -> Int)
    ]

  conjureWith args{usePatterns = False} "fib01" fib01
    [ pr (0::Int)
    , prim "+" ((+) :: Int -> Int -> Int)
    , prim "dec" (subtract 1 :: Int -> Int)
    , prim "<=" ((<=) :: Int -> Int -> Bool)
    ]
-- expected function:
-- fib01 x y z  =  if z <= 0 then y else fib01 y (x + y) (dec z)
--                 1  2 3  4      5      6     7  8 9 10  11 12
