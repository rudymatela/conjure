-- 2021  Colin Runciman
-- updated in 2024 to use the recent interface of Conjure
import Conjure
import Prelude hiding (sum, take, drop)

sumSpec :: ([Int] -> Int) -> Bool
sumSpec sum  =  and [ sum []       ==  0
                    , sum [0,1]    ==  1
                    , sum [1,0,1]  ==  2 ]

-- hoping for something like
-- sum xs = if null xs then 0 else head xs + sum (tail xs)

sumPrimitives :: [Prim]
sumPrimitives =
  [ prim "null" (null :: [Int] -> Bool)
  , pr (0::Int)
  , prim "+"    ((+) :: Int -> Int -> Int)
  , prim "head" (head :: [Int] -> Int)
  , prim "tail" (tail :: [Int] -> [Int])
  ]

appSpec :: ([Int] -> [Int] -> [Int]) -> Bool
appSpec (++)  =  and [ []    ++ [0,1]  ==  [0,1]
                     , [0,1] ++ []     ==  [0,1]
                     , [0,1] ++ [0,1]  ==  [0,1,0,1] ]

-- hoping for something like
-- app xs ys = if null xs then ys else head xs : app (tail xs) ys

appPrimitives :: [Prim]
appPrimitives =
  [ prim "null" (null :: [Int] -> Bool)
  , prim ":"    ((:) :: Int -> [Int] -> [Int])
  , prim "head" (head :: [Int] -> Int)
  , prim "tail" (tail :: [Int] -> [Int])
  ]

memSpec :: (Int -> [Int] -> Bool) -> Bool
memSpec mem  =  and [ 0 `mem` []       ==  False
                    , 0 `mem` [0,1,1]  ==  True
                    , 0 `mem` [1,0,1]  ==  True
                    , 0 `mem` [1,1,0]  ==  True
                    , 0 `mem` [1,1,1]  ==  False ]

-- hoping for something like
-- mem x xs = not (null xs) && (x == head xs || mem x (tail xs))

memPrimitives :: [Prim]
memPrimitives =
  [ pr False
  , pr True
  , pr ([] :: [Int])
  , prim ":" ((:) :: Int -> [Int] -> [Int])
  , prim "null" (null :: [Int] -> Bool)
  , prim "==" ((==) :: Int -> Int -> Bool)
  , prim "not" (not :: Bool -> Bool)
  , prim "&&" ((&&) :: Bool -> Bool -> Bool)
  , prim "||" ((||) :: Bool -> Bool -> Bool)
  , prim "head" (head :: [Int] -> Int)
  , prim "tail" (tail :: [Int] -> [Int])
  ]

setSpec :: ([Int] -> Bool) -> Bool
setSpec set  =  and [ set []     ==  True
                    , set [0]    ==  True
                    , set [0,0]  ==  False
                    , set [0,1]  ==  True
                    , set [0,1,2] == True
                    , set [0,0,1] == False
                    , set [0,1,0] == False
                    , set [0,1,1] == False ]

-- hoping for something like
-- set xs = null xs || not (elem (head xs) (tail xs)) && set (tail xs)

setPrimitives :: [Prim]
setPrimitives =
  [ prim "null" (null :: [Int] -> Bool)
  , pr False
  , pr True
  , pr ([] :: [Int])
  , prim ":" ((:) :: Int -> [Int] -> [Int])
  , prim "not" (not :: Bool -> Bool)
  , prim "&&" ((&&) :: Bool -> Bool -> Bool)
  , prim "||" ((||) :: Bool -> Bool -> Bool)
  , prim "head" (head :: [Int] -> Int)
  , prim "tail" (tail :: [Int] -> [Int])
  , prim "elem" (elem :: Int -> [Int] -> Bool)
  ]

takeSpec :: (Int -> [Int] -> [Int]) -> Bool
takeSpec take  =  and [ take 0 []     ==  []
                      , take 1 []     ==  []
                      , take 0 [0,1]  ==  []
                      , take 1 [0,1]  ==  [0]
                      , take 2 [0,1]  ==  [0,1]
                      , take 3 [0,1]  ==  [0,1] ]

-- hoping for something like
-- take n xs = if n==0 || null xs then [] else head xs : take (dec n) (tail xs)

takePrimitives :: [Prim]
takePrimitives =
  [ pr (0 :: Int)
  , pr ([] :: [Int])
  , prim "null" (null :: [Int] -> Bool)
  , prim "==" ((==) :: Int -> Int -> Bool)
  , prim "||" ((||) :: Bool -> Bool -> Bool)
  , prim "dec" ((\n -> n-1) :: Int -> Int)
  , prim ":" ((:) :: Int -> [Int] -> [Int])
  , prim "head" (head :: [Int] -> Int)
  , prim "tail" (tail :: [Int] -> [Int])
  ]

dropSpec :: (Int -> [Int] -> [Int]) -> Bool
dropSpec drop  =  and [ drop 0 []     ==  []
                      , drop 1 []     ==  []
                      , drop 0 [0,1]  ==  [0,1]
                      , drop 1 [0,1]  ==  [1]
                      , drop 2 [0,1]  ==  []
                      , drop 3 [0,1]  ==  [] ]

-- hoping for something like
-- drop n xs = if n==0 || null xs then xs else drop (dec n) (tail xs)

dropPrimitives :: [Prim]
dropPrimitives =
  [ pr (0 :: Int)
  , pr ([] :: [Int])
  , prim ":" ((:) :: Int -> [Int] -> [Int])
  , prim "null" (null :: [Int] -> Bool)
  , prim "==" ((==) :: Int -> Int -> Bool)
  , prim "||" ((||) :: Bool -> Bool -> Bool)
  , prim "dec" ((\n -> n-1) :: Int -> Int)
  , prim "tail" (tail :: [Int] -> [Int])
  ]

main :: IO ()
main = do
  conjureFromSpecWith args{maxSize=15} "sum" sumSpec sumPrimitives
  conjureFromSpecWith args{maxSize=15} "app" appSpec appPrimitives
  conjureFromSpecWith args{maxSize=15} "mem" memSpec memPrimitives
  conjureFromSpecWith args{maxSize=15} "set" setSpec setPrimitives
  conjureFromSpecWith args{maxSize=20} "take" takeSpec takePrimitives
  conjureFromSpecWith args{maxSize=15} "drop" dropSpec dropPrimitives

