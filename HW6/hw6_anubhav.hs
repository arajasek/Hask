fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


fibs1 :: [Integer]
fibs1 = map fib [0..]


fibs2 :: [Integer]
fibs2 = (map fst (scanl (\(minus2, minus1) _ -> (minus1, minus2 + minus1)) (0,1) [0..]))


data Stream a = Cons a (Stream a)


streamToList :: Stream a -> [a]
streamToList (Cons el rest) = el : (streamToList rest)

instance Show a => Show (Stream a) where
	show strm = show (take 20 (streamToList strm))

streamRepeat :: a -> Stream a
streamRepeat element = Cons element (streamRepeat element)


streamMap :: (a -> b) -> Stream a -> Stream b
streamMap fn (Cons element rest) = Cons (fn element) (streamMap fn rest)


streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed generate seed = Cons seed (streamFromSeed generate (generate seed))


nats :: Stream Integer
nats = streamFromSeed (\x -> (x+1)) 0


highestDivisorHelper :: Integer -> Integer -> Integer
highestDivisorHelper n power
	| n `mod` 2 == 0 = (highestDivisorHelper (n `div` 2) (power + 1))
	| otherwise = power

highestDivisor :: Integer -> Integer
highestDivisor n = highestDivisorHelper n 0

ruler :: Stream Integer
ruler = streamMap highestDivisor (streamMap (\x -> (x+1)) nats)

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons e1 rest) x = Cons e1 (interleaveStreams x rest1)


--getRuler :: Stream Integer
--streamMap streamFromSeed (streamMap (\x -> (x+1)) nats))

