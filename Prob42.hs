{-
The n^(th) term of the sequence of triangle numbers is given by, t_(n) = ½n(n+1); so the first ten triangle numbers are:

1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

By converting each letter in a word to a number corresponding to its alphabetical position and adding these values we form a word value. For example, the word value for SKY is 19 + 11 + 25 = 55 = t_(10). If the word value is a triangle number then we shall call the word a triangle word.

Using words.txt (right click and 'Save Link/Target As...'), a 16K text file containing nearly two-thousand common English words, how many are triangle words?
 -}

module Main where

import Data.Char(ord)

main = do
	wdata <- readFile "words.txt"
	print $ solve wdata

solve ws = foldl step 0 (foldr splits [""] ws)
    where
	splits x ac@(a:as)
            | x == ',' = "" : ac
            | x == '\"' = ac
	    | otherwise = (x : a) : as
		
        step a x | isTrig $ csum x = a + 1
                 | otherwise = a
	csum = foldl (\a x -> a + ord x - 64) 0
	isTrig t = let n = (sqrt (1 + 8 * fromIntegral t) - 1) / 2
		   in fromIntegral (round n) == n

{- Old variant.
solve s = foldl (\a x -> if check x then a + 1 else a) 0 (wsums s)
    where
	check = check' (twords 1)
	check' (t:ts) ws
		| ws == t = True
		| ws < t = False
		| otherwise = check' ts ws

wsums ws = map csum (wlist ws)
    where
	wlist = foldr splits [""]
	splits x ac@(a:as)
		| x == ',' = "" : ac
		| x == '\"' = ac
		| otherwise = (x : a) : as
	csum = foldl (\a x -> a + ord x - 64) 0

twords n = floor (0.5 * n * (n + 1)) : twords (n + 1)
-}
