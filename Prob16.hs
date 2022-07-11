

powerDigitSum :: Integer -> Integer
powerDigitSum n = powerDigitSum' (2^n)
  where powerDigitSum' 0 = 0
        powerDigitSum' k = k `mod` 10 + powerDigitSum' (k `div` 10)