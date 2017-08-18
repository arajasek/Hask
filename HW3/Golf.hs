module Golf where

skips' :: [a] -> Int -> Int -> [a]
skips' [] _ _ = []
skips' (x:xs) i n | mod i n == 0 = x:(skips' xs (i + 1) n)
		  | otherwise    = skips'  xs (i + 1) n

skips :: [a] -> [[a]]
skips x = map (skips' x 1) [1..(length x)]
