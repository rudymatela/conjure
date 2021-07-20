-- longshot.hs: miscellaneous longshots
--
-- Copyright (C) 2021 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure

sort' :: [Int] -> [Int]
sort' []       =  []
sort' [x]      =  [x]
sort' [x,y]
  | x <= y     =  [x,y]
  | otherwise  =  [y,x]
sort' [x,y,z]
  | x <= y && y <= z  =  [x,y,z]
  | z <= y && y <= x  =  [z,y,x]

pow :: Int -> Int -> Int
pow 2 0  =  1
pow 2 1  =  2
pow 2 2  =  4
pow 2 3  =  8
pow 3 2  =  9

duplicates :: [Int] -> [Int] -- Eq a => [a] -> [a]
duplicates []  =  []
duplicates (x:xs)  =
  if x `elem` xs && not (x `elem` d)
  then x : d
  else d
  where
  d  =  duplicates xs

positionsFrom :: Int -> Int -> [Int] -> [Int]
positionsFrom n x  =  from n
  where
  from _ []  =  []
  from n (y:ys)  =  if y == x
                    then n : f
                    else f
    where
    f  =  from (n+1) ys

main :: IO ()
main = do
  -- TODO: review longshots when using case patterns

  -- qsort
  -- qsort xs  =  if null xs                                 -- 3
  --              then []                                    -- 4
  --              else qsort (filter (< head xs) (tail xs))  -- 11
  --                ++ (head xs:[])                          -- 16
  --                ++ qsort (filter (>= head xs) (tail xs)) -- 24
  -- not only this is out of reach performance wise,
  -- but the needed recursive calls will not be enumerated
  conjureWithMaxSize 8 "qsort" sort'  -- OOM when bigger, TODO: why?
    [ pr ([] :: [Int])
    , prim ":" ((:) :: Int -> [Int] -> [Int])
    , prim "++" ((++) :: [Int] -> [Int] -> [Int])
    , prim "<" ((<) :: Int -> Int -> Bool)
    , prim ">=" ((>=) :: Int -> Int -> Bool)
    , prim "filter" (filter :: (Int -> Bool) -> [Int] -> [Int])
    ]

  -- pow b e  =  if e == 0 then 1 else b * pow b (dec e)
  --             1  2  3 4      5      6 7 8   9  10 11
  -- somehow this takes 30s to run, the two arguments
  -- of the same type introduce the difficulty here.
  conjureWithMaxSize 8 "pow" pow
    [ pr (0::Int)
    , pr (1::Int)
    , prim "*" ((*) :: Int -> Int -> Int)
    , prim "dec" (subtract 1 :: Int -> Int)
    ]

  -- pow b e  =  if e == 0 then 1 else pow b (halve e) * pow b (halve e) * if odd e then b else 1
  --             1  2  3 4      5      6   7  8     9 10 11 12  13   14 15 16 17  18    19     20
  -- -- OR --
  -- pow b 0  =  1
  -- pow b e  =  pow b (halve e) * pow b (halve e) * if odd e then b else 1
  --             2   3  4     5  6 7   8  9    10 11 12 13 14     15     16
  -- out of reach performance wise
  conjureWithMaxSize 6 "pow" pow
    [ pr (0::Int)
    , pr (1::Int)
    , prim "+" ((+) :: Int -> Int -> Int)
    , prim "*" ((*) :: Int -> Int -> Int)
    , prim "halve" ((`div` 2) :: Int -> Int)
    ]

  -- duplicates xs  =
  --   if null xs                                                                   --  3
  --   then []                                                                      --  4
  --   else if head xs `elem` tail xs && not (head xs `elem` duplicates (tail xs))  -- 18
  --        then head xs : duplicates (tail xs)                                     -- 24
  --        else duplicates (tail xs)                                               -- 27
  -- -- OR --
  -- duplicates []  =  []                                                  --  1
  -- duplicates (x:xs)  =  if x `elem` xs && not (x `elem` duplicates xs)  -- 11
  --                       then x : duplicates xs                          -- 15
  --                       else duplicates xs                              -- 17
  -- out of reach memory and performance wise
  conjure "duplicates" duplicates
    [ pr ([] :: [Int])
    , pr True
    , pr False
    , prim "not" not
    , prim "||" (||)
    , prim "&&" (&&)
    , prim ":" ((:) :: Int -> [Int] -> [Int])
    , prim "elem" (elem :: Int -> [Int] -> Bool)
    ]

  conjureWithMaxSize 9 "positionsFrom" positionsFrom
    [ pr ([] :: [Int])
    , pr True
    , pr False
    , prim "not" not
    , prim "||" (||)
    , prim "&&" (&&)
    , prim ":" ((:) :: Int -> [Int] -> [Int])
    , prim "==" ((==) :: Int -> Int -> Bool)
    ]
