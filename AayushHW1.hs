toDigitsRev::Integer -> [Integer]
toDigitsRev n
 | (n <= 0) = []
 | otherwise = (mod n 10) : toDigitsRev (div n 10)

revList::[Integer] -> [Integer]
revList [] = []
revList (n:l) = l ++ [n]

toDigits::Integer -> [Integer]
toDigits l = revList (toDigitsRev l)

intListLength :: [Integer] -> Integer
intListLength []     = 0
intListLength (x:xs) = 1 + intListLength xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:l)
 | mod (intListLength l) 2 == 0 = x:(doubleEveryOther l)
 | otherwise = (x*2):(doubleEveryOther l)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (n:l)
 | n < 10 = n + (sumDigits l)
 | otherwise = (sumDigits (toDigits n)) + (sumDigits l)

validate :: Integer -> Bool
validate n
 | mod (sumDigits (doubleEveryOther (toDigits n) ) ) 10 == 0 = True
 | otherwise = False

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
 | (n <= 0) = []
 | otherwise = (hanoi (n-1) a c b) ++ [(a,b)] ++ (hanoi (n-1) c b a)

data FailableDouble = Failure
                    | OK Double
  deriving Show

data AlgDataType = Constr1 Integer Integer
                 | Constr2 String
                 | Constr1 Integer Integer Integer
                 | Constr4
