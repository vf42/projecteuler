module Main where

import Data.Char (ord)
import Data.Bits ((.|.), shiftL)

main = do
  putStrLn $ show solve

-- Build Fibonacci sequence.
fib :: [Integer]
fib = 1 : fib' 1 1
    where
      fib' a b = b : fib' b (a + b)

-- Find first Fibonacci number to match a criteria.
solve :: Int
solve = solve' 1 fib
    where
      solve' k (f:fs) = let s = show f
                        in
                          if isPandigit' f && isPandigit (take 9 s)
                          then k
                          else solve' (k + 1) fs
               

-- Test a list of chars to contain all the digits 1..9.
isPandigit :: [Char] -> Bool
isPandigit cs = 1022 == foldr 
                (\c flag -> flag .|. 1 `shiftL` (ord c - 48)) 
                (0::Int) cs

-- Test an integer to have all the digits 1..9 on the last 1..9 (optimized).
isPandigit' :: Integer -> Bool
isPandigit' x = 
    let (y, flag) = foldr 
                    (\_ (x, flag) -> (x `div` 10, 
                                       flag .|. 1 `shiftL` (x `mod` 10)))
                    (fromInteger (x `mod` 1000000000)::Int, 0::Int) [1..9]
    in 1022 == flag