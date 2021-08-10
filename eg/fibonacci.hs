-- fibonacci.hs: conjuring a fibonacci function
import Conjure

fibonacci :: Int -> Int
fibonacci 0  =  1
fibonacci 1  =  1
fibonacci 2  =  2
fibonacci 3  =  3
fibonacci 4  =  5
fibonacci 5  =  8
fibonacci 6  =  13

main :: IO ()
main  =  do
  conjure "fibonacci n" fibonacci
    [ pr (0::Int)
    , pr (1::Int)
    , prim "+" ((+) :: Int -> Int -> Int)
    , prim "dec" (subtract 1 :: Int -> Int)
    ]
-- expected function:
-- fibonacci n  =  if n <= 1 then 1 else fibonacci (dec n) + fibonacci (dec (dec n))
--                 1  2 3  4      5      6          7   8  9        10  11   12  13
