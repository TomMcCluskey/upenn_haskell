{-# OPTIONS_GHC -Wall #-}
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0    = []
  | otherwise = digits n
  where digits 0 = []
        digits x = digits (x `div` 10) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | otherwise = digits n
  where digits 0 = []
        digits x = x `mod` 10 : digits (x `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n = reverse (doubleFromStart (reverse n))
  where doubleFromStart (x:y:zs) = x : [2*y] ++ doubleFromStart zs
        doubleFromStart q = q

-- sumDigits is equal to the sum of applying toDigits to each item
-- in a list and then summing those digits
-- map should sum the return of toDigits for each list item
-- then that new mapped list should be summed
sumDigits :: [Integer] -> Integer
sumDigits n = sum (concatMap toDigits n)

validate :: Integer -> Bool
validate n = (sumDigits (doubleEveryOther (toDigits n))) `mod` 10 == 0

-- concat' :: [[a]] -> [a]
-- concat' = foldr f []
--     where f as bs = as ++ bs

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 o x t = [(o,x)]
hanoi 2 o x t = [(o,t), (o,x), (t,x)]
hanoi n o x t = hanoi (n-1) o t x ++ hanoi 1 o x t ++ hanoi (n-1) t x o
