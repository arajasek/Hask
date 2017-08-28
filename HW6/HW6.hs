import Prelude
import Data.Foldable

-- 1

fib :: Integer -> Integer
fib n
 | (n == 0) = 0
 | (n == 1) = 1
 | otherwise = fib (n-1) + fib (n-2)

fibs1 = map fib [0..]

--2

--this seems right, but it doesn't work...take 10 fibs2 is blank?

growFibs :: [Integer] -> Integer -> [Integer]
growFibs [] _ = [0, 1]
growFibs l _ = (l ++ [last l + last (init l)]) 

fibs2 :: [Integer]
fibs2 = foldl' growFibs [] [1..]

--3

data Stream t = C t (Stream t)
 deriving Show

streamToList :: Stream a -> [a]
streamToList (C n s) = n:(streamToList s)

instance Show a => Show (Stream a) where
 show l = take 20 (streamToList l)
