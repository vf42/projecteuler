{-
I started with the simplest problems trying to make a one-line Haskell solution for them JFF.
-}

-- Prob 1
(sum . filter (\k -> k `mod` 3 == 0 || k `mod` 5 == 0)) [1..999]

-- Prob 2
let fib a b = a : fib b (a + b) in (sum . filter (\a -> a `mod` 2 == 0) . takeWhile (<=4000000)) (fib 1 2)

-- Prob 5
let nums = [1..100] in (sum nums)^2 - (sum . map (^2)) nums

-- Prob 4
let isPlndr n = show n == (reverse . show) n in foldr (\v m -> if v > m then v else m) 0 $ filter isPlndr (reverse [a*b | a <- [100..999], b <- [100..a]])

-- Prob 9
let k = 12 in let (a, b, c) = (head . filter (\(a, b, c) -> a^2 + b^2 == c^2)) [(a, b, c) | c <- [2..(k-2)], b <- [1..(min (c-1) (k-c))], a <- [k-b-c], a+b+c == k] in a*b*c

-- Prob 16
let f k = if k == 0 then 0 else k `mod` 10 + f (k `div` 10) in f (2^15)