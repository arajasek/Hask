import Data.List

-- Exercise 1
-- Original, for testing:
fun1Orig :: [Integer] -> Integer
fun1Orig [] = 1
fun1Orig (x:xs)
  | even x = (x - 2) * fun1Orig xs
  | otherwise = fun1Orig xs

fun1 :: [Integer] -> Integer
fun1 = foldr (*) 1 . map (subtract 2) . filter even

-- Original, for testing:
fun2Orig :: Integer -> Integer
fun2Orig 1 = 0
fun2Orig n
  | even n = n + fun2Orig (n `div` 2)
  | otherwise = fun2Orig (3 * n + 1)

collatz :: Integer -> Integer
collatz n
  | even n = n `div` 2
  | otherwise = 3 * n + 1

fun2 :: Integer -> Integer
fun2 = foldr (+) 0 . filter even . takeWhile (>1) . iterate collatz

-- Exercise 2
data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insertBalanced Leaf

insertBalanced :: a -> Tree a -> Tree a
insertBalanced e Leaf = Node 0 Leaf e Leaf
insertBalanced e (Node h l root r)
  | ((height r) < (height l)) || ((nodes r) < (nodes l)) = (Node h l root (insertBalanced e r))
  | ((height l) < (height r)) || ((nodes l) < (nodes r)) = (Node h (insertBalanced e l) root r)
  | otherwise = (Node (h+1) (insertBalanced e l) root r)

height :: Tree a -> Integer
height Leaf = -1
height (Node h _ _ _) = h

nodes :: Tree a -> Integer
nodes Leaf = 0
nodes (Node _ l _ r) = 1 + (nodes l) + (nodes r)

-- Exercise 3
xor :: [Bool] -> Bool
xor = foldr (\b1 b2 -> (b1 || b2) && not (b1 && b2)) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> (f x) : y) []

-- Exercise 4
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2*x+1) ([1..n] \\ marked)
  where marked = filter (\x -> x <= n)
          (map (\p -> case p of (a, b) -> a + b + 2 * a * b)
            (filter (\p -> case p of (a, b) -> a + b + 2 * a * b <= n)
              [(i, j) | i <- [1..n], j <- [1..n], i <= j]))