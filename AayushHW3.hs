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

count   :: Eq a => a -> [a] -> Int
count x =  length . filter (==x)

highestFrequency :: [Integer] -> Integer -> Int -> Int
highestFrequency l m currbes
 | (m > 9) = currbes
 | (count m l) > currbes = (highestFrequency l (m+1) (count m l))
 | otherwise = (highestFrequency l (m+1) currbes)


processList :: [Integer] -> Integer -> Int -> String
processList l m n
 | (n <= 0) = []
 | (m > 9) = ('\n':(processList l 0 (n-1)))
 | (count m l) >= n = ('*' : (processList l (m+1) n))
 | otherwise = (' ' : (processList l (m+1) n))


histogram :: [Integer] -> String
histogram l = (processList l 0 (highestFrequency l 0 0)) ++ "==========\n0123456789\n"
