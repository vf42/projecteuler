module Main where

import Utils

limit = 28123::Int

main = do
  print $ sol

sol = 
    let 
        ab = allAbundand
        xs = filter (not . canBeSum ab) [1..limit]
    in foldl (+) 0 xs

{- Bottleneck :-( -}
canBeSum :: [Int] -> Int -> Bool
canBeSum ab = canBeSum' ab
    where
      canBeSum' bs@(a:as) x | a < x = if checkSums bs 
                                      then True 
                                      else canBeSum' as x
                            | otherwise = False
          where
            checkSums (b:bs) | a + b == x = True
                             | a + b > x = False
                             | otherwise = checkSums bs

{-allSums :: Int -> [Int]
allSums mxs = let (frst, dfs) = abDiff
              in allSums' frst dfs
    where
      allSums' curr dfs | curr >= mxs = []
                        | otherwise = 

abDiff :: (Int, [Int])
abDiff = (first, abDiff' abund)
    where
      abund = allAbundand
      first = head abund
      abDiff' (a:as) = (a - first) : abDiff' as-}

allAbundand :: [Int]
allAbundand = filter isAbundand [1..]

isAbundand :: Int -> Bool
isAbundand x = x < (foldl (+) 0 (propDiv x))

propDiv :: Int -> [Int]
propDiv x = let x' = floor (sqrt (fromIntegral x))
            in filter (\d -> x `mod` d == 0) [1..x']
