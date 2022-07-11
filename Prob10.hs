import Utils

summationOfPrimes :: Int -> Int
summationOfPrimes k = foldr (+) 0 $ takeWhile ((>) k) seedPrimes