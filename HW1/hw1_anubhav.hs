toDigitsHelper :: Integer -> [Integer] -> [Integer]
toDigitsHelper 0 ls = ls
toDigitsHelper n ls = toDigitsHelper (n `div` 10) ((n `mod` 10) : ls)


toDigits :: Integer -> [Integer]
toDigits n
	| n <= 0 = []
	| True = toDigitsHelper n []


toDigitsRev :: Integer -> [Integer]
toDigitsRev n
	| n <= 0 = []
	| True = (n `mod` 10) : toDigitsRev (n `div` 10)


doubleHelp :: [Integer] -> Bool -> [Integer]
doubleHelp [] _ = []
doubleHelp (x:xs) tf
	| tf == True = (2*x) : doubleHelp xs False
	| otherwise = x : doubleHelp xs True

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther ls = doubleHelp ls (length ls `mod` 2 == 0)


sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (sum (toDigitsRev x)) + (sumDigits xs)


validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0


--------------------------------------------------------------------


type Peg = String
type Move = (Peg, Peg)

-- the assignment says the stacks are in the order a,c,b but that's
-- because the assignment is stupid
-- curr, temp, target
hanoih :: Integer -> Peg -> Peg -> Peg -> [Move] -> [Move]
hanoih 0 _ _ _ acc = acc
hanoih n a b c acc = hanoih (n-1) a c b ((a,c) : hanoih (n-1) b a c acc)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c = hanoih n a b c []