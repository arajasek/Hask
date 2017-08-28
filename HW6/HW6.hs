{-# LANGUAGE FlexibleInstances #-}

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

growFibs :: Integer -> Integer -> [Integer]
growFibs a b = a : (growFibs b (a+b))

fibs2 :: [Integer]
fibs2 = growFibs 0 1

--this seems right, but it doesn't work...take 10 fibs2 is blank?

-- growFibs :: [Integer] -> Integer -> [Integer]
-- growFibs [] _ = [0, 1]
-- growFibs l _ = (l ++ [last l + last (init l)]) 

-- fibs2 :: [Integer]
-- fibs2 = foldl' growFibs [] [1..]

--3

data Stream t = C t (Stream t)

streamToList :: Stream a -> [a]
streamToList (C n s) = n:(streamToList s)

instance Show a => Show (Stream a) where
 show l = show (take 20 (streamToList l))

 --4

streamRepeat :: a -> Stream a
streamRepeat n = C n (streamRepeat n)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (C n l) = C (f n) (streamMap f l)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f n = C n (streamFromSeed f (f n))

--5

nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (C n1 l1) l2 = C n1 (interleaveStreams l2 l1)

buildRuler :: Integer -> Stream Integer
buildRuler n = interleaveStreams (streamRepeat n) (buildRuler (n + 1))

ruler = buildRuler 0

--6

x :: Stream Integer
x = C 0 (C 1 (streamRepeat 0))

instance Num (Stream Integer) where
 fromInteger n = C n (streamRepeat 0)
 negate = streamMap (* (-1))
 (C n1 l1) + (C n2 l2) = C (n1 + n2) (l1 + l2)  
 (C n1 l1) * (C n2 l2) = C (n1 * n2) (streamMap (* n1) l2 + (l1 * (C n2 l2)))  

instance Fractional (Stream Integer) where
 (C n1 l1) / (C n2 l2) = C (div n1 n2) (streamMap (`div` n2) (l1 - (C n1 l1) / (C n2 l2) * l2))

fibs3 = x / (1 - x - x*x)

--7

data Matrix = M Integer Integer Integer Integer
instance Num (Matrix) where
 (M a1 b1 c1 d1) * (M a2 b2 c2 d2) = M (a1*a2 + b1*c2) (a1*b2 + b1*d2) (c1*a2 + d1*c2) (c1*b2 + c1*d2)

f = M 1 1 1 0

proj :: Matrix -> Integer
proj (M a b c d) = a

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = proj (f^n)