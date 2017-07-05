module Main where

import Utils

import Data.Char (digitToInt)

limit = 1000000

main = do
  print $ solve2 limit

solve2 :: Int -> [Int]
solve2 limit = 2 : 3 : 5 : 7 
               :  foldl updateCircular [] 
                     (dropWhile (<10) (takeWhile (<limit) seedPrimes))
    where
      updateCircular acc p 
          | p `elem` acc || not (checkDigits p) = acc
          | otherwise = 
              let rots = allRot p
                  prim = p : takeWhile (isPrime) (tail rots)
              in if length prim == length rots 
                 then prim ++ acc 
                 else acc
      checkDigits 0 = True
      checkDigits p             
          | (p `mod` 10) `elem` [1, 3, 7, 9] = True 
                                               && checkDigits (p `div` 10) 
          | otherwise = False
      

allRot :: Int -> [Int]
allRot x = x : next (rot' x)
    where
      degree' = degree x
      rot' x = (x `mod` degree') * 10 + (x `div` degree')
      next y | x == y = []
             | otherwise = y : next (rot' y)
            

{- Find the answer for a problem. 
   Returns a list of all suitable primes. 
   Performance: about 5 seconds. -}
solve :: Int -> [Int]
solve limit = filter isCircular (takeWhile (<limit) seedPrimes)
    where 
      isCircular p = checkDigits p && isCircular' (rotD p)
          where 
            checkDigits p 
                | p < 10    = True
                | otherwise = checkDigits' p
                where 
                  checkDigits' 0 = True
                  checkDigits' p | (p `mod` 10) `elem` [1, 3, 7, 9] 
                                     = True && checkDigits' (p `div` 10) 
                                 | otherwise = False
            isCircular' q
                | q == p       = True
                | q < degree p = False
                | otherwise    = isPrime q && isCircular' (rotD q)


{- Decimal rotation. It's enough with 1 sign rotation for the task. -}
rotD x = rotateD' (degree x)
    where 
      rotateD' d = (x `mod` d) * 10 + (x `div` d) 

{- Calculates a degree of a number for decimal rotation.
   Degree is 10^n where n is max possible for which x div 10^n /= 0 -}
degree x = degree' 1
    where 
      degree' d | x `div` d == 0 = d `div` 10 
                | otherwise      = degree' (d * 10)
