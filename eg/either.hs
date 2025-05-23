-- either.hs: conjuring functions over either values
--
-- Copyright (C) 2024-2025 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure

isLeft' :: Either A A -> Bool
isLeft' (Left 0)  =  True
isLeft' (Left 1)  =  True
isLeft' (Right 0)  =  False
isLeft' (Right 1)  =  False

isRight' :: Either A A -> Bool
isRight' (Left 0)  =  False
isRight' (Left 1)  =  False
isRight' (Right 0)  =  True
isRight' (Right 1)  =  True

fromLeft' :: A -> Either A A -> A
fromLeft' 0 (Left 1)  =  1
fromLeft' 0 (Left 2)  =  2
fromLeft' 1 (Left 0)  =  0
fromLeft' 0 (Right 1)  =  0
fromLeft' 1 (Right 0)  =  1

fromRight' :: A -> Either A A -> A
fromRight' 0 (Left 1)  =  0
fromRight' 0 (Left 2)  =  0
fromRight' 1 (Left 0)  =  1
fromRight' 0 (Right 1)  =  1
fromRight' 1 (Right 0)  =  0

eitherSpec :: ((A -> A) -> (A -> A) -> Either A A -> A) -> [Property]
eitherSpec either  =
  [ property $ either (+1) (+2) (Left 0) == 1
  , property $ either (+1) (+2) (Right 0) == 2
  , property $ either (*10) (*100) (Left 1) == 10
  , property $ either (*10) (*100) (Right 2) == 200
  ]

lefts' :: [Either A A] -> [A]
lefts' []  =  []
lefts' [Right 0]  =  []
lefts' [Left 0, Right 1]  =  [0]
lefts' [Right 0, Left 1]  =  [1]
lefts' [Left 0, Left 1]  =  [0,1]

main :: IO ()
main = do
  conjure "isLeft" isLeft' ingredients
  conjure "isRight" isRight' ingredients
  conjure "fromLeft" fromLeft' ingredients
  conjure "fromLeft" fromRight' ingredients
  conjureFromSpec "either" eitherSpec ingredients
  conjure "lefts" lefts' ingredients

ingredients :: [Ingredient]
ingredients  =
  [ fun "Left"  (Left :: A -> Either A A)
  , fun "Right" (Right :: A -> Either A A)

  , unfun False
  , unfun True

  , unfun ([] :: [A])
  , fun ":" ((:) :: A -> [A] -> [A])
  ]
