module Main where

upper = 30

main = do
     print $ comp4 upper

comp4 mxx = let primes = (genPrimes (mxx `div` 2))
            in comp4' (head primes) primes
      where
        comp4' _ [] = 0
        comp4' p ps = let pss = dropBig ps
                      in (+) (length pss) (comp4' (head pss) pss) 
               where 
                     dropBig xs = dropBig' (last xs) (init xs)
                     dropBig' _ [] = []
                     dropBig' y ys | y * p >= mxx = dropBig' (last ys) (init ys)
                                   | otherwise = 
                                     dropBig' (last ys) (init ys) ++ [y]
                     --dropBig = dropWhile (\x -> x * p >= mxx)

{- Generate chain of primes up to mp -}
genPrimes :: Int -> [Int]
genPrimes mp = let pseed = 2:[]
               in genPrimes' 3 pseed
     where
        genPrimes' p ps | p < mp && test ps = genPrimes' (p + 2) (ps ++ [p])
                        | p >= mp = ps
                        | otherwise = genPrimes' (p + 2) ps
                where
                        test xs = test' (floor (sqrt (fromIntegral p))) xs
                        test' lim [] = True
                        test' lim (x:xs) | p > lim = True
                                         | p <= lim && p `mod` x == 0 = False
                                         | otherwise = test' lim xs

{- Running isPrime only once for each number,
   dropping unneccessary (too big) primes
   and not performing too much multiplication. -}
comp3 mxx = (1::Int) + comp3' 3 (2:[])
      where
        mxp = mxx `div` 2
        comp3' p ps | p < mxp && isPrime p = 
                                         let pss = dropBig (p:ps)
                                         in (+) (length pss)
                                                (comp3' (p + 2) pss)
                    | p >= mxp = 0
                    | otherwise = comp3' (p + 2) ps
                    where
                        dropBig = dropWhile (\x -> x * p >= mxx)

{- FUCKING BOTTLENECK, think of better prime generation. 
   TODO: generate primes explicitly and use ONLY primes for checks. -}       
isPrime :: Int -> Bool
isPrime n = (n `mod` 2 /= 0) && checkOdd 3 (floor (sqrt (fromIntegral n)))
        where
                checkOdd k lim | k > lim = True
                               | k <= lim && n `mod` k == 0 = False
                               | otherwise = checkOdd (k + 2) lim 


{- Little bit better, but also too slow. -}
{-comp2 mxx = comp2' 2
      where
        mxp = (floor (sqrt (fromInteger mxx)))
        comp2' p | p < mxp && isPrime p = comp2'' p
                 | p >= mxp = []
                 | otherwise = comp2' (p + 1)
                 where
                        comp2'' q | p * q < mxx && (p == q || isPrime q) = 
                                          (p * q) : comp2'' (q + 1)
                                  | p * q >= mxx = comp2' (p + 1)
                                  | otherwise = comp2'' (q + 1) 
-}
{- Old slow version.
composites mxx = nextcomp primes
           where
                mxp = mxx `div` 2
                nextcomp (p:ps) | p < mxp = nextcomp' $ primes' p
                                | otherwise = []
                    where 
                          nextcomp' (q:qs) | p * q < mxx = (p*q) : nextcomp' qs
                                           | otherwise = nextcomp ps

primes = primes' 2

primes' n | isPrime n = n : primes' (n + 1)
          | otherwise = primes' (n + 1)

isPrime n = checkDiv 2 (n `div` 2)
        where
                checkDiv k lim | k > lim = True
                               | k <= lim && n `mod` k == 0 = False
                               | otherwise = checkDiv (k+1) lim
-}                
       