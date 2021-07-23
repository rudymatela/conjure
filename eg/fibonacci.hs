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
fibonacci 7  =  21

main :: IO ()
main  =  do
  -- finds a implementation in about 20s to run with maxSize = 11 or 12
  conjureWith args{maxSize = 10} "fibonacci n" fibonacci
    [ pr (0::Int)
    , pr (1::Int)
    , prim "dec" (subtract 1 :: Int -> Int)
    , prim "+" ((+) :: Int -> Int -> Int)
    ]
-- expected function:
-- fibonacci n  =  if n <= 1 then 1 else fibonacci (dec n) + fibonacci (dec (dec n))
--                 1  2 3  4      5      6          7   8  9        10  11   12  13


{- to note, if dec appears later than + in the primitives list:

conjureWith ...  =

> print $ canReduceTo thy (xx -+- dec xx) (dec (dec xx))
False
> print $ canReduceTo thy (dec (dec xx)) (xx -+- dec xx)
True

-}
