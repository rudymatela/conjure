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

fib01 :: Int -> Int -> Int -> Int
fib01 0 1 0  =  1
fib01 0 1 1  =  1
fib01 0 1 2  =  2
fib01 0 1 3  =  3
fib01 0 1 4  =  5
fib01 0 1 5  =  8
fib01 0 1 6  =  13
fib01 0 1 7  =  21

as :: Args
as  =  args{maxSize=13,maxRecursiveCalls=2}

main :: IO ()
main  =  do
  conjureWith as "fibonacci n" fibonacci
    [ val (1::Int)
    , value "+" ((+) :: Int -> Int -> Int)
    , value "dec" (subtract 1 :: Int -> Int)
    , value "<=" ((<=) :: Int -> Int -> Bool)
    ]
-- expected function:
-- fibonacci n  =  if n <= 1 then 1 else fibonacci (dec n) + fibonacci (dec (dec n))
--                 1  2 3  4      5      6          7   8  9        10  11   12  13

  -- TODO: make so that Conjure is able to produce fib01.
  --       the descending arguments change made it so that
  --       fib01 is not discoverable anymore
  conjureWithMaxSize 10 "fib01" fib01
    [ val (0::Int)
    , value "+" ((+) :: Int -> Int -> Int)
    , value "dec" (subtract 1 :: Int -> Int)
    , value "<=" ((<=) :: Int -> Int -> Bool)
    ]
-- desired function:
-- fib01 x y z  =  if z <= 0 then y else fib01 y (x + y) (dec z)
--                 1  2 3  4      5      6     7  8 9 10  11 12
