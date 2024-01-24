-- id.hs: conjuring the identity and const functions
--
-- Copyright (C) 2024 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Conjure

main :: IO ()
main = do
  -- conjure needs no primitives
  -- to figure out the implementation of id and const
  conjure "id"    (id :: Int -> Int)           []
  conjure "const" (const :: Int -> Int -> Int) []
