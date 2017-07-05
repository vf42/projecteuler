{-
  http://projecteuler.net/index.php?section=problems&id=50
-}

module Main where

limit = 1000000

main = do
  print $ solve seedPrimes limit

{- 
   Brute-forse solution - O(n^2), n - number of primes. 
   TODO: rewrite using folds.
 -}
solve :: [Int] -> Int -> (Int, Int)
solve ps limit = solve' (takeWhile (< limit) ps)
    where
      solve' [] = (0, 0)
      solve' (p:ps) =
          let
              (s1, c1) = solve'' (p, 1) ps
              (s2, c2) = solve' ps
          in if c1 >= c2 then (s1, c1) else (s2, c2)
          where
            solve'' (s, c) [] 
                    | s < limit && isPrime s = (s, c + 1)
                    | otherwise = (0, 0)
            solve'' (s, c) (p:ps)
                    | s >= limit = (0, 0)
                    | s < limit && isPrime s = 
                        let (s1, c1) = solve'' (s + p, c + 1) ps
                        in if s1 > 0 then (s1, c1) else (s, c)
                    | otherwise = solnext
                where solnext = (solve'' (s + p, c + 1) ps)

{- Prime seed. -}
seedPrimes :: [Int]
seedPrimes = 2 : filter isPrime [3,5..]
 
{- Prime check. -}
isPrime :: Int -> Bool
isPrime n | n == 2 = True
          | otherwise = (n `mod` 2 /= 0) 
                  && checkOdd 3 (floor (sqrt (fromIntegral n)))
        where
                checkOdd k lim | k > lim = True
                               | k <= lim && n `mod` k == 0 = False
                               | otherwise = checkOdd (k + 2) lim 