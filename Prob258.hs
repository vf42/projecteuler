module Main where

main = do
  putStrLn $ show $ g' (10^8)

g k | k < 2000 = 1
    | otherwise = g (k - 2000) + g (k - 1999)

g' k = (mypow 2 (k `div` 2000)) `mod` 20092010

mypow x k | k == 0 = 1
          | otherwise = mypow x (k - 1) `seq` 
                        ((x * mypow x (k - 1)) `mod` 20092010)

{-
LOL

*Main Data.List> g 2000
2
*Main Data.List> g 4000
4
*Main Data.List> g 6000
8
*Main Data.List> g 7000
8
*Main Data.List> g 8000
16
*Main Data.List> g 10000
32
*Main Data.List> g 12000
64
*Main Data.List> g 14000
128
-}