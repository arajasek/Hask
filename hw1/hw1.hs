-- Exercise 1
toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = rem n 10 : toDigitsRev (div n 10)

-- Exercise 2
doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = []
doubleEveryOtherRev [x] = [x]
doubleEveryOtherRev (x:y:zs) = (doubleEveryOtherRev zs) ++ (2*y : x : [])

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther ds = (doubleEveryOtherRev (reverse ds))

-- Exercise 3
sumDigitsHelper :: [Integer] -> Integer
sumDigitsHelper [] = 0
sumDigitsHelper (d:ds) = d + sumDigitsHelper ds

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sumDigitsHelper(toDigits x) + sumDigits xs

-- Exercise 4
validate :: Integer -> Bool
validate n = rem (sumDigits(doubleEveryOther(toDigits n))) 10 == 0

-- Exercise 5
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n p1 p2 p3 = (hanoi (n-1) p1 p3 p2)
                   ++ [(p1, p2)]
                   ++ (hanoi (n-1) p3 p2 p1)