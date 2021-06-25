-- Computes averages of several benchmarks
--
-- Copyright (C) 2021 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Data.List
import Data.Function
import Text.Printf

groupOn f  =  groupBy ((==) `on` f)

parseLine :: String -> (String,Double)
parseLine s  =  (n,t)
  where
  n  =  unwords $ init ws
  t  =  read $ last ws
  ws  =  words s

showLine :: (String,Double) -> String
showLine (s,x)  =  printf "%-25s %.2f" s x

collapseAvg :: [(String,Double)] -> (String,Double)
collapseAvg sxs  =  (fst $ head sxs, avg $ map snd sxs)

avg :: [Double] -> Double
avg xs  =  sum xs / fromIntegral (length xs)

io :: String -> String
io  =  unlines
    .  map showLine
    .  map collapseAvg
    .  groupOn fst
    .  map parseLine
    .  lines

main :: IO ()
main  =  interact io
