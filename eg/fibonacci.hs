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
