skips :: [a] -> [[a]]
skips w = [[s | (f,s) <- zip [1..] w, f `mod` n == 0] | n <- [1..length w]]


-- skips word = [[second | (first, second) <- zip [1..] word, first `mod` n == 0] | n <- [1..length word]]


localMaxima :: [Integer] -> [Integer]
localMaxima l = [a | (a,b,c) <- scanl (\(d,p,_) n -> (p, n, d < p && p > n)) (0,0,False) (tail l), c]

-- localMaxima l = [a | (a,b,isPrevLocalMax) <- scanl (\(prevprev,prev,_) curr -> (prev,curr,prevprev<prev && prev>curr)) (0,0,False) (tail l), isPrevLocalMax]

h :: [Int] -> Int -> String
h l n
	| n > (maximum l) = []
	| True = (h l (n+1)) ++ (foldl (\a c -> a ++ (if (c >= n) then "*" else " ")) "" l) ++ "\n"

--writeLine l n
--	| n > (maximum l) = []
--	| True = (writeLine l (n+1)) ++ (foldl (\acc curr -> acc ++ (if (curr >= n) then "*" else " ")) "" l) ++ "\n"


histogram :: [Integer] -> String
histogram l = (h [(length (filter (== n) l)) | n <- [0..9]] 1) ++ "==========\n0123456789\n"