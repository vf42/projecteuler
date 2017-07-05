module Main where

main = do
  putStrLn $ show solve

-- Upper search limit.
maxNum = 10000

-- Count Lychrel numbers below given limit.
solve = length (allLychrel maxNum)

-- Get all Lychrel numbers below given limit.
allLychrel limit = [x | x <- [1..limit], isLychrel x]

-- Upper iteration limit.
maxSteps = 50

-- Check that number is Lychrel (within iteration limit).
isLychrel x = check 2 (next x)
    where
      check step y | step >= maxSteps = True
                   | isPalindrome y = False
                   | otherwise = check (step + 1) (next y)
      next x = x + invert x

-- Check that number is palindrome.
isPalindrome x = 0 == x - invert x

-- Reverse number.
invert = read . reverse . show