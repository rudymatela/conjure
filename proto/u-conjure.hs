-- u-conjure.hs -- u-Conjure
--
-- This is a prototype for Conjure, a library for conjuring code
-- out of partially implemented functions.
--
--
-- Copyright (C) 2021 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
--
--
-- To run this you need to have both LeanCheck and Express installed:
--
-- $ cabal install leancheck
-- $ cabal install express
--
-- If installation fails, use v1-install:
--
-- $ cabal v1-install leancheck
-- $ cabal v1-install express
import Data.List
import Data.Maybe
import Data.Express
import Test.LeanCheck.Error

square :: Int -> Int
square 0  =  0
square 1  =  1
square 2  =  4
square 3  =  9
square 4  =  16

add :: Int -> Int -> Int
add 0 0  =  0
add 0 1  =  1
add 1 0  =  1
add 1 1  =  2

fact :: Int -> Int
fact 0  =  1
fact 1  =  1
fact 2  =  2
fact 3  =  6
fact 4  =  24

second :: [Int] -> Int
second [x,y]  =  y
second [x,y,z]  =  y
second [x,y,z,w]  =  y

main :: IO ()
main  =  do
  value "square" (square :: Int -> Int) `conjureFrom` intBackground
  value "add" (add :: Int -> Int -> Int) `conjureFrom` intBackground
  value "fact" (fact :: Int -> Int) `conjureFrom` intBackground

  value "==>" (==>) `conjureFrom`
    [ val False
    , val True
    , value "not" not
    , value "&&" (&&)
    , value "||" (||)
    ]

  value "second" (second :: [Int] -> Int) `conjureFrom` listBackground
  value "reverse" (reverse :: [Int] -> [Int]) `conjureFrom` listBackground
  where
  intBackground :: [Expr]
  intBackground  =
    [ val (0 :: Int)
    , val (1 :: Int)
    , val (2 :: Int)
    , val (3 :: Int)
    , value "+" ((+) :: Int -> Int -> Int)
    , value "*" ((*) :: Int -> Int -> Int)
    , value "-" ((-) :: Int -> Int -> Int)
    ]
  listBackground :: [Expr]
  listBackground  =
    [ val (0 :: Int)
    , val (1 :: Int)
    , val ([] :: [Int])
    , value "head" (head :: [Int] -> Int)
    , value "tail" (tail :: [Int] -> [Int])
    , value ":" ((:) :: Int -> [Int] -> [Int])
    , value "++" ((++) :: [Int] -> [Int] -> [Int])
    ]


conjureFrom :: Expr -> [Expr] -> IO ()
ff `conjureFrom` es  =  do
  print ff  -- prints the type signature
  case rs of
    []    -> putStrLn $ "cannot conjure"
--  es    -> putStrLn $ unlines $ map showEq es  -- uncomment to show all found variations
    (e:_) -> putStrLn $ showEq e
  putStrLn ""
  where
  nm (Value s _)  =  s
  rrff  =  ('_':nm ff) `varAsTypeOf` ff
  rs  =  [ ffxx -==- e
         | e <- candidateExprsFrom $ [rrff] ++ xxs ++ filter isGround es
         , isWellTyped (ffxx -==- e)
         , isTrue (ffxx -==- e)
         , isDefined e
         ]
  ffxx  =  mostGeneralCanonicalVariation
        .  fromJust
        $  application ff es
  (_:xxs)  =  unfoldApp ffxx
  showEq eq  =  showExpr (lhs eq) ++ "  =  " ++ showExpr (rhs eq)


application :: Expr -> [Expr] -> Maybe Expr
application ff es  =  appn ff
  where
  appn ff
    | isFun ff   =  case [e | Just (_ :$ e) <- (map (ff $$)) es] of
                    [] -> Nothing  -- could not find type representative in es
                    (e:_) -> appn (ff :$ holeAsTypeOf e)
    | otherwise  =  Just ff


candidateExprsFrom :: [Expr] -> [Expr]
candidateExprsFrom  =  concat . take 6 . expressionsT
  where
  expressionsT ds  =  [ds] \/ (delay $ productMaybeWith ($$) es es)
    where
    es = expressionsT ds


isTrue :: Expr -> Bool
isTrue  =  all (errorToTrue . eval False) . take 60 . grounds


-- checks if a function is defined for at least one combination of argument values
isDefined :: Expr -> Bool
isDefined e  =  any (isJust . errorToNothing . eval False) . take 60 . grounds $ (e -==- e)


(-==-) :: Expr -> Expr -> Expr
ex -==- ey  =  headOr (val False) . map (:$ ey) $ mapMaybe ($$ ex)
  [ value "==" ((==) :: Int -> Int -> Bool)
  , value "==" ((==) :: Bool -> Bool -> Bool)
  , value "==" ((==) :: [Int] -> [Int] -> Bool)
  , value "==" ((==) :: [Bool] -> [Bool] -> Bool)
  ]
  where
  headOr x []     =  x
  headOr _ (x:_)  =  x

lhs, rhs :: Expr -> Expr
lhs (((Value "==" _) :$ e) :$ _)  =  e
rhs (((Value "==" _) :$ _) :$ e)  =  e


grounds :: Expr -> [Expr]
grounds e  =  map (e //-)  .  concat
           $  products [mapT ((,) v) (tiersFor v) | v <- nubVars e]


tiersFor :: Expr -> [[Expr]]
tiersFor e  =  case show (typ e) of
  "Int"    ->  mapT val (tiers `asTypeOf` [[undefined :: Int]])
  "Bool"   ->  mapT val (tiers `asTypeOf` [[undefined :: Bool]])
  "[Int]"  ->  mapT val (tiers `asTypeOf` [[undefined :: [Int]]])
  "[Bool]" ->  mapT val (tiers `asTypeOf` [[undefined :: [Bool]]])
  _        ->  []
