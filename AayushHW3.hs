module Golf where

chomp :: [a] -> Int -> Int -> [a]
chomp [] _ _ = []
chomp (b:l) m n
 | (m == n) = b: ( chomp l m 0 )
 | otherwise = (chomp l m (n+ 1))

skipLength :: [a] -> Int -> [[a]]
skipLength [] _ = []
skipLength (b:l) n = (chomp (b:l) n n) : ( skipLength l (n + 1))

skips :: [a] -> [[a]]
skips [] = []
skips l = skipLength l 0

localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:l) 
 | (b > a) && (b > c) = (b : localMaxima (b:c:l))
 | otherwise = localMaxima(b:c:l)
localMaxima _ = []