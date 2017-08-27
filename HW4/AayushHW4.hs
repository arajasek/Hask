import Data.List

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
 | even x = (x - 2) * fun1 xs
 | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter (even)

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n 
 | even n = n + fun2 (n `div` 2)
 | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (> 1) . 
 iterate (\x -> (mod x 2) * (3 * x + 1) + (1 - (mod x 2)) * (div x 2))

data Tree a = Leaf
 | Node Integer (Tree a) a (Tree a)
 deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf = -1
height (Node n _ _ _) = n

population :: Tree a -> Integer
population Leaf = 0
population (Node _ l _ r) = population l + population r + 1

buildHelper :: a -> Tree a -> Tree a
buildHelper p Leaf = Node 0 Leaf p Leaf
buildHelper p (Node x l t r)
 | ((height l > height r) || (population l > population r))
 =  (Node x l t (buildHelper p r))
 | ((height r > height l) || (population r > population l) || (population l < 2 ^ (height l + 1) - 1))
 =  (Node x (buildHelper p l) t r)
 | otherwise = (Node (x+1) (buildHelper p l) t r)


foldTree :: [a] -> Tree a
foldTree = foldr buildHelper Leaf

doXor :: Bool -> Bool -> Bool
doXor False p = p
doXor True p = not p


xor :: [Bool] -> Bool
xor = foldr doXor False

doMap :: (a -> b) -> a -> [b] -> [b]
doMap f a l = (f a) : l

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (doMap f) []

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

listOneToN :: Integer -> [Integer]
listOneToN n = takeWhile (<= n) (iterate (\x -> x + 1) 1)

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2*x + 1) (
 listOneToN n \\ 
 (map (\(i,j) -> (i+j+2*i*j))
 (cartProd (listOneToN n) (listOneToN n))))



