module Main where

import Data.List (sortBy)

main = do
  putStrLn $ show $ solve2 12000

lower = 1.0 / 3.0
upper = 1.0 / 2.0

solve2 = countRpfs

-- 1000 in 0.3s, 12000 in 46s with this one, probably could do better.
countRpfs k = length [(n, d) | d <- [1..k], n <- [1..(d - 1)], 
                                   gcd n d == 1 && test (n, d)]
    where
      test (n, d) = let v = (fromInteger n) / (fromInteger d)
                    in v > lower && v < upper

{- WAY TOO BAD. 2 seconds for d <= 1000.
-- Bounds of search.
lower = (1, 3)
upper = (1, 2)

-- Find number of fractions between lower and upper bounds.
solve = length . takeWhile (/= upper) . tail . dropWhile (/= lower) . sortedRpf

-- Sort RPFs.
sortedRpf = sortBy cmp . allRpf
    where
      cmp a b | val a < val b = LT
              | otherwise = GT
      val (n, d) = (fromInteger n) / (fromInteger d)

-- Get all reduced proper fractions for d <= k.
allRpf k = [(n, d) 
                | d <- [1..k], n <- [1..(d - 1)], gcd n d == 1] 
-}