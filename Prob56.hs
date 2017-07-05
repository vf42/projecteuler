module Main where

import Data.List (maximum)

main = do
  putStrLn $ show solve

lowa = 90
lowb = 90
upper = 99

-- Maximal sum of digits for all a^b, a, b < upper
solve = maximum [ maximum [dsum (a ^ b) | b <- [lowb..upper]] 
                      | a <- [lowa..upper] ]

-- Sum of all digits.
dsum x | x == 0 = 0
       | otherwise = x `mod` 10 + dsum (x `div` 10)