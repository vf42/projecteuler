{-
  http://projecteuler.net/index.php?section=problems&id=54
-}

import System.Environment (getArgs)
import Control.Monad (liftM)

main = do
  args <- getArgs
  case args of
    [input] -> do
              input' <- readFile input
              putStrLn $ show $ (solve . lines) input'
    _ -> putStrLn "Enter input file"


--
-- Cards and operations on them.
--

data Value = V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9 | V10 | VJ | VQ | VK | VA
           deriving (Show, Ord, Eq, Enum)

data Suit = Spades | Hearts | Diamonds | Clubs
          deriving (Show, Ord, Eq)

data Card = Card Value Suit
          deriving (Show, Ord, Eq)

card :: String -> Card
card (v:'H':[]) = Card (card' v) Hearts
card (v:'S':[]) = Card (card' v) Spades
card (v:'D':[]) = Card (card' v) Diamonds
card (v:'C':[]) = Card (card' v) Clubs

card' :: Char -> Value
card' '2' = V2
card' '3' = V3
card' '4' = V4
card' '5' = V5
card' '6' = V6
card' '7' = V7
card' '8' = V8
card' '9' = V9
card' 'T' = V10
card' 'J' = VJ
card' 'Q' = VQ
card' 'K' = VK
card' 'A' = VA

value (Card v _) = v
suit (Card _ s) = s


--
-- Hand construction.
--

type Hand = [Card]

hand :: [String] -> Hand
hand cs = orderHand (map card cs)

orderHand :: Hand -> Hand
orderHand = foldr (\c h -> order c h) []
    where
      order c [] = c:[]
      order c (h:hs) | c <= h = c : h : hs
                     | otherwise = h : (order c hs)


-- 
-- Hand ranking.
--

data Rank = HighCard Value 
          | OnePair Value
          | TwoPairs Value Value 
          | Three Value
          | Straight Value
          | Flush Suit
          | FullHouse Rank Rank -- Three and OnePair 
          | Four Value
          | StraightFlush Rank Rank -- Straight and Flush
          | RoyalFlush Suit
            deriving (Ord, Eq, Show)

data RankedHand = RH Rank Hand
                deriving (Show)

-- Checking for straight.
guessStraight h = guessStraight' (value (head h)) (tail h)
    where
      guessStraight' v (c:cs) 
          | succ v == (value c) = guessStraight' (value c) cs
          | otherwise = Nothing
      guessStraight' v [] = Just (Straight v)

-- Checking for flush.
guessFlush h = guessFlush' (head h) (tail h)
    where
      guessFlush' x (y:cs) | suit x == suit y = guessFlush' y cs
                           | otherwise = Nothing
      guessFlush' x [] = Just (Flush (suit x))

-- Checking if it's value of sequence type (Straight/Flush and combinations).
guessSeq :: Hand -> (Maybe RankedHand)
guessSeq h = guessSeq' (guessStraight h) (guessFlush h)
    where
      guessSeq' (Just st@(Straight v)) (Just fl@(Flush s))
                | v == VA = Just (RH (RoyalFlush s) [])
                | otherwise = Just (RH (StraightFlush st fl) [])
      guessSeq' (Just st) _ = Just (RH st [])
      guessSeq' _ (Just fl) = Just (RH fl [])
      guessSeq' _ _ = Nothing

-- Checking if it's group of cards (Pairs, Threes, etc.).
guessGroup :: Hand -> RankedHand
guessGroup h = 
    guessGroup' (consGr (consGr (map (\c -> HighCard (value c)) h) []) [])
        where
          guessGroup' rs@((HighCard v):rss) =
              let cs = (orderHand 
                        (map (\(HighCard v) -> Card v Spades) (tail rs)))
              in RH (HighCard (value (last cs))) (init cs)
          guessGroup' rs =
                  RH (head rs)
                         (orderHand 
                          (map (\(HighCard v) -> Card v Spades) (tail rs)))

-- On queue of group ranks, construct new ranks.
consGr (a@(HighCard av) : []) closed = closed ++ [a]
consGr (a:[]) closed = a:closed

consGr (a@(HighCard av) : b@(HighCard bv) : os) cs
    | av == bv = consGr ((OnePair av) : os) cs
    | otherwise = consGr (b:os) (a:cs)

consGr (a@(OnePair av) : b@(HighCard bv) : os) cs
    | av == bv = consGr ((Three av) : os) cs
    | otherwise = consGr (b : os ++ [a]) cs
consGr ((OnePair av) : (OnePair bv) : os) cs = 
    consGr (os ++ [TwoPairs av bv]) cs
consGr (a@(OnePair av) : b@(Three bv) : os) cs = consGr (b:a:os) cs

consGr (a@(Three av) : b@(HighCard bv) : os) cs
    | av == bv = consGr ((Four av) : os) cs
    | otherwise = consGr (b : os ++ [a]) cs
consGr (a@(Three av) : b@(OnePair bv) : os) cs = 
    consGr (os ++ [FullHouse a b]) cs

consGr (a:os) closed = consGr os (a:closed)

-- Get hand's rank.
rankHand :: Hand -> RankedHand
rankHand h = case (guessSeq h) of
               (Just rh) -> rh
               Nothing -> guessGroup h 

-- Compare two hands.
compareHands :: Hand -> Hand -> Ordering
compareHands a b = let rh1 = rankHand a
                       rh2 = rankHand b
                   in compareHands' rh1 rh2
    where
      compareHands' (RH r1 h1) (RH r2 h2)
          | r1 /= r2 = compare r1 r2
          | otherwise = cmpHigh h1 h2
      cmpHigh h1 h2 | last h1 == last h2 = cmpHigh (init h1) (init h2)
                    | otherwise = compare (last h1) (last h2)

--
-- Main solution routine: read both player's hands and choose winner.
--
solve ss = length (filter (==GT) (map procLine ss))
    where
      procLine s = procLine' (words s)
          where
            procLine' cs = let h1 = hand (take 5 cs)
                               h2 = hand (drop 5 cs)
                           in compareHands h1 h2