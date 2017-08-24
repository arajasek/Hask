-- Exercise 1
fun1 :: [Integer] -> [Integer]
fun1 = map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 = sum . takeWhile even . iterate (\n -> n `div` 2) 

-- Exercise 2
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

insertElement :: a -> Tree a -> Tree a
insertElement a Leaf = Node 0 Leaf a Leaf
insertElement a t@(Node n _ _ _) = Node (n + 1) Leaf a t

foldTree :: [a] -> Tree a
foldTree = foldr insertElement Leaf

-- Exercise 3
xor :: [Bool] -> Bool
xor = foldr (\p q -> (p || q) && not (p && q)) False

xor' :: [Bool] -> Bool
xor' = odd . length . filter id

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> (f x):y) []

-- Exercise 4
cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

intPairs :: Integer -> [(Integer, Integer)]
intPairs n = cartProd  (enumFromTo 1 n)  (enumFromTo 1 n)

strikeOuts = map (\(x,y) -> x + y + 2*x*y) . filter (\(x,y) -> x <= y) . intPairs

-- sieveSundaram = map (\x -> 2 * x + 1) . 
