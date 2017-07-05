module Main where

main = do
  putStrLn $ show $ solve 100
  
{-
x = n * i (i = 2..?)
y = x / (i - 1)
Heuristic: max i = n + 1 (FAILED)
-}
  
solve :: Int -> Int
solve k = undefined {- solve' 2
  where
    solve' n = -}
          
solve' n = foldr step [] [2..]
  where
    step i rs = step' (n * i)
      where
        step' x | x `mod` (i - 1) == 0 = (x, x `div` (i - 1)) : rs
                | otherwise = rs