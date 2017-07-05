module Main where

main = do
  print $ solution limit

limit = 1500000

solution topL = foldr step 0 [12..topL]
    where
      step l acc = step' (rightTriangles l)
          where step' (rt:[]) = 1 + acc
                step' _ = acc

{- First brute-forse solution, O(l^2)
rightTriangles :: Int -> [(Int, Int, Int)]
rightTriangles l = foldr stepA [] [1..l']
    where
      l' = (l `div` 2) - 1
      stepA a acc = acc ++ foldr stepB [] [a..l']
          where
            stepB b acc | goodC (c2) = (a, b, floor (sqrt' c2)) : acc
                        | otherwise = acc
                where
                  sqrt' = sqrt . fromIntegral
                  c2 = a^2 + b^2
                  goodC c2 = let cf = sqrt' c2
                                 c = floor cf
                             in isRound cf && c > b && c <= l' + 1 &&
                                a + b + c == l
-}

{- A little better, O(l) -}
rightTriangles :: Integer -> [(Integer, Integer, Integer)]
rightTriangles l = take 2 (foldr stepC [] [5 .. l'])
    where
      l' = l `div` 2
      stepC c acc = let u = 2.0
                        v = fromIntegral (-2 * (l - c))
                        w = fromIntegral (l^2 - 2 * l * c)
                    in stepC' (solveEq u v w)
          where
            stepC' (a:b:[]) | isRound a && isRound b 
                              && a > 0 && b > 0 = 
                                (floor a, floor b, c) : acc
            stepC' _ = acc
      
solveEq :: Double -> Double -> Double -> [Double]
solveEq a b c = 
    let d = b^2 - 4 * a * c
    in solveEq' d
    where
      solveEq' d | d < 0 = []
                 | otherwise = let answer f = (-b `f` sqrt d) / (2 * a)
                               in [answer (+), answer (-)]

isRound x = floor x == ceiling x