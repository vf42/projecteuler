module Main where

import List (union)
import Utils (seedPrimes)

main = do
  putStrLn $ show $ solve 4

-- Heuristic limit which we'll go to in seeding primes.
walktop = 1000

-- Limited prime number list we'll operate on.
primes = takeWhile (<= walktop) seedPrimes

-- Find the first N consecutive integers to have N distinct primes factors.
solve :: Int -> Int
solve n | n < 2 = -1
        | otherwise = walk [n..]
    where
      walk (c:cs) 
          | n == length (takeWhile (exactFact n) [c..]) = c
          | otherwise = walk cs

-- Check if number has exactly N prime factors.
exactFact :: Int -> Int -> Bool
exactFact n x = n == length [f | f <- primes, x `mod` f == 0]
