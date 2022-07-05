{-
I started with the simplest problems trying to make a one-line Haskell solution for them JFF.
-}

-- Prob 1
(sum . filter (\k -> k `mod` 3 == 0 || k `mod` 5 == 0)) [1..999]

-- Prob 2
let fib a b = a : fib b (a + b) in (sum . filter (\a -> a `mod` 2 == 0) . takeWhile (<=4000000)) (fib 1 2)

-- Prob 5
let nums = [1..100] in (sum nums)^2 - (sum . map (^2)) nums

