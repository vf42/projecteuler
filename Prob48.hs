module Main where

main = do 
  print $ niceResult solution

limit = 1000
lastDigits = 10
modulo = 10 ^ lastDigits

niceResult :: Int -> [Char]
niceResult result = niceResult' (show result)
    where
      niceResult' s | length s < lastDigits = niceResult' ('0':s)
                    | otherwise = s

solution :: Int
solution = foldr step 0 [1..limit]
    where
      step x acc = (acc + (modProd x)) `mod` modulo

modProd :: Int -> Int
modProd x = foldr (\a -> \acc -> acc * x `mod` modulo) x [1 .. (x - 1)]
