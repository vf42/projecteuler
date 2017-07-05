{-- The first known prime found to exceed one million digits was discovered in 1999, and is a Mersenne prime of the form 2^(6972593)−1; it contains exactly 2,098,960 digits. Subsequently other Mersenne primes, of the form 2^(p)−1, have been found which contain more digits.

However, in 2004 there was found a massive non-Mersenne prime which contains 2,357,207 digits: 28433×2^(7830457)+1.

Find the last ten digits of this prime number.
--}

module Main where

import Data.Bits (shiftL, shiftR)

main = do 
  print $ niceResult solution

-- The value is n * 2^m + 1
n = 28433
m = 7830457

lastDigits = 10
modulo = 10 ^ lastDigits

niceResult result = niceResult' (show result)
    where
      niceResult' s | length s < lastDigits = niceResult' ('0':s)
                    | otherwise = s

solution = (n * (big2exp m) + 1) `mod` modulo

big2exp e | e <= little = 2 ^ e
          | e `mod` 2 == 0 = nextexp ^ 2 `mod` modulo
          | otherwise = 2 * big2exp (e - 1) `mod` modulo
    where
      little = 8
      nextexp = big2exp (e `div` 2)
