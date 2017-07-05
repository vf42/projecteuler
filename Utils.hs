module Utils where

{- Prime seed. -}
seedPrimes :: [Int]
seedPrimes = 2 : filter isPrime [3,5..]

{- Prime check. -}
isPrime :: Int -> Bool
isPrime n | n == 2 = True
          | otherwise = (n `mod` 2 /= 0) 
                  && checkOdd 3 (floor (sqrt (fromIntegral n)))
        where
                checkOdd k lim 
                    | k > lim                    = True
                    | k <= lim && n `mod` k == 0 = False
                    | otherwise                  = checkOdd (k + 2) lim 

-- Get all prime factors of a number.
primeFactors :: Int -> [Int]
primeFactors x = foldr step [] (takeWhile (<= x `div` 2) seedPrimes)
    where
      step p fs | x `mod` p == 0 = p : fs
                | otherwise = fs
