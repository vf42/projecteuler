module Main where

import Data.List (union, nub)

import Utils (seedPrimes)

main = do
  putStrLn $ solve

-- Do The Job.
solve = let result = head (filter (\xs -> xs /= [] && head xs /= 1487) 
                           (map getSeq (permPrimes primes)))
        in concat (map show result)

-- Extract subset of primes that we need.
primes = dropWhile (< 100000) . takeWhile (< 1000000) $ seedPrimes

-- Check that one number is permutation of other's digits.
isPerm x y = let x' = show x
                 y' = show y
             in
               (length . nub $ x') == (length . nub $ y')
                       && [] == [z | z <- show x, not . elem z $ show y]

-- Get permutating primes.
permPrimes (p:ps) | ps' /= [] && 2 <= length ps' = (p : ps') : permPrimes ps
                  | otherwise = permPrimes ps
    where
      ps' = [x | x <- ps, isPerm p x]
permPrimes _ = []

-- Try to extract some 3-member algebraic sequence from a list.
getSeq (x:y:xs) = let zs = [z | z <- xs, z - y == y - x]
                  in if zs /= [] then x : y: zs else getSeq (x:xs)
getSeq _ = []