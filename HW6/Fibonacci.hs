{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

-- Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 = map fib [0..]

-- Exercise 2
fibs2 = map (\(_, x) -> x) $ iterate (\(x, y) -> (x + y, x)) (1, 0)

-- Exercise 3

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x:(streamToList xs)

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) (Cons y ys) = Cons x $ Cons y $ interleaveStreams xs ys

-- Exercise 6
x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

scalarMult :: Integer -> Stream Integer -> Stream Integer
scalarMult n (Cons x xs) = Cons (n * x) $ scalarMult n xs

scalarDiv :: Stream Integer -> Integer -> Stream Integer
scalarDiv (Cons x xs) n = Cons (div x n) $ scalarDiv xs n

instance Num (Stream Integer) where
  fromInteger n = Cons n $ streamRepeat 0
  negate (Cons x xs) = Cons (-x) (negate xs)
  (+) (Cons x xs) (Cons y ys) = Cons (x + y) $ xs + ys
  (*) x_all@(Cons x xs) y_all@(Cons y ys) = Cons (x * y) $ scalar + (xs * y_all)
    where scalar = scalarMult x ys

instance Fractional (Stream Integer) where
  (/) x_all@(Cons x xs) y_all@(Cons y ys) = Cons (div x y) $ scalarDiv (xs - ys * (x_all / y_all)) y

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

-- Exercise 7
data Matrix = Matrix Integer Integer Integer Integer
  deriving Show

instance Num Matrix where
  (*) (Matrix a1 a2 a3 a4) (Matrix b1 b2 b3 b4) = Matrix (a1 * b1 + a2 * b3)
                                                         (a1 * b2 + a2 * b4)
							 (a3 * b1 + a4 * b3)
                                                         (a3 * b2 + a4 * b4)

fibMatSeed = Matrix 1 1 1 0

matElA :: Matrix -> Integer
matElA (Matrix a _ _ _) = a

fib4 :: Integer -> Integer
fib4 = matElA . (^) fibMatSeed
