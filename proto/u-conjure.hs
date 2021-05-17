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
import Data.Typeable
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

reverse' :: [Int] -> [Int]
reverse' []  =  []
reverse' [x]  =  [x]
reverse' [x,y]  =  [y,x]
reverse' [x,y,z]  =  [z,y,x]
reverse' [x,y,z,w]  =  [w,z,y,x]
reverse' [x,y,z,w,v]  =  [v,w,z,y,x]
reverse' [x,y,z,w,v,u]  =  [u,v,w,z,y,x]


main :: IO ()
main  =  do
  conjure "square" square intBackground
  conjure "add"    add    intBackground
  conjure "fact"   fact   intBackground

  conjure "==>" (==>)
    [ val False
    , val True
    , value "not" not
    , value "&&" (&&)
    , value "||" (||)
    ]

  conjure "second"  second   listBackground
  conjure "reverse" reverse' listBackground
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


conjure :: Typeable f => String -> f -> [Expr] -> IO ()
conjure nm f primitives  =  do
  print ff  -- prints the type signature
  case rs of
    []    -> putStrLn $ "cannot conjure"
--  es    -> putStrLn $ unlines $ map showEq es  -- uncomment to show all found variations
    (e:_) -> putStrLn $ showEq e
  putStrLn ""
  where
  (ff,rs)  =  conjpure nm f primitives
  showEq eq  =  showExpr (lhs eq) ++ "  =  " ++ showExpr (rhs eq)


conjpure :: Typeable f => String -> f -> [Expr] -> (Expr,[Expr])
conjpure nm f primitives  =  (ff,rs)
  where
  ff  =  value nm f
  rs  =  [ ffxx -==- e
         | e <- candidateExprsFrom $ xxs ++ filter isGround primitives
         , isWellTyped (ffxx -==- e)
         , isTrue (ffxx -==- e)
         , isDefined e
         ]
  ffxx  =  mostGeneralCanonicalVariation
        $  application ff primitives
  (_:xxs)  =  unfoldApp ffxx


definedBinds :: Expr -> [[(Expr,Expr)]]
definedBinds ffxx  =  [bs | bs <- bss, errorToFalse . eval False $ e //- bs]
  where
  e  =  ffxx -==- ffxx
  bss  =  take 360 $ groundBinds ffxx


application :: Expr -> [Expr] -> Expr
application ff es  =  appn ff
  where
  appn ff | isFun ff   =  case [e | Just (_ :$ e) <- (map (ff $$)) es] of
                          [] -> error "application: could not find type representative"
                          (e:_) -> appn (ff :$ holeAsTypeOf e)
          | otherwise  =  ff


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
grounds e  =  map (e //-) $ groundBinds e


groundBinds :: Expr -> [[(Expr,Expr)]]
groundBinds e  =  concat $ products [mapT ((,) v) (tiersFor v) | v <- nubVars e]


tiersFor :: Expr -> [[Expr]]
tiersFor e  =  case show (typ e) of
  "Int"    ->  mapT val (tiers `asTypeOf` [[undefined :: Int]])
  "Bool"   ->  mapT val (tiers `asTypeOf` [[undefined :: Bool]])
  "[Int]"  ->  mapT val (tiers `asTypeOf` [[undefined :: [Int]]])
  "[Bool]" ->  mapT val (tiers `asTypeOf` [[undefined :: [Bool]]])
  _        ->  []
