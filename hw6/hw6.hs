{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

-- Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2
fibs2 :: [Integer]
fibs2 = fibAcc [0..]

-- Is there a good 'recursion pattern' that this can be expressed as?
-- Not sure how to achieve the overlapping w/ stuff in Prelude
fibAcc :: [Integer] -> [Integer]
fibAcc (0:1:1:ns) = 0 : fibAcc (1:1:ns)
fibAcc (0:1:ns) = fibAcc (0:1:1:ns)
fibAcc (a:b:c:ns) = a : fibAcc(b:(a+b):ns)

-- Exercise 3
data Stream a = StreamCons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (StreamCons a rest) = a : streamToList rest

instance Show a => Show (Stream a) where
  show s = show (take 20 (streamToList s))

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat e = StreamCons e (streamRepeat e)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (StreamCons e rest) = StreamCons (f e) (streamMap f rest)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed uf s = StreamCons s (streamFromSeed uf (uf s))

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = iteratedInterleave (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (StreamCons e rest) s = StreamCons e (interleaveStreams s rest)

iteratedInterleave :: (a -> a) -> a -> Stream a
iteratedInterleave f e = interleaveStreams (streamRepeat e) (iteratedInterleave f (f e))

-- Exercise 6
x :: Stream Integer
x = StreamCons 0 (StreamCons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n = StreamCons n (streamRepeat 0)
  negate = streamMap (0-)
  (StreamCons a as) + (StreamCons b bs) = StreamCons (a+b) (as + bs)
  (StreamCons a as) * b_@(StreamCons b bs) = StreamCons (a*b) ((streamMap (*a) bs) + (as * b_))

instance Fractional (Stream Integer) where
  a_@(StreamCons a as) / b_@(StreamCons b bs) = StreamCons (a `div` b) (streamMap (`div` b) (as - (a_/b_)*bs))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

-- Exercise 7
data Matrix = Matrix ((Integer, Integer), (Integer, Integer))

instance Num Matrix where
  (Matrix ((a, b), (c, d))) * (Matrix ((e, f), (g, h)))
    = Matrix ((a*e + b*g, a*f + b*h), (c*e + d*g, c*f + d*h))

instance Show Matrix where
  show (Matrix ((a, b), (c, d))) = show [[a, b], [c, d]]

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = case result of (Matrix ((_, f_n), _)) -> f_n
  where result = Matrix ((1, 1), (1, 0)) ^ n