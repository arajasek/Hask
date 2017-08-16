-- Exercise 1
-- Filters out only elements that occur at the nth positions in the list.
-- Input list has elements paired w/ their (1-based) index. NB: Indices need
-- to be 1-based so that the mod does the right thing (0 === 0 for all moduli,
-- which is not what we want).
keepNths :: Int -> [(Int, a)] -> [a]
keepNths n xs = map snd (filter (\x -> rem (fst x) n == 0) xs)

-- Map keepNths over the list [1, ..., |xs|], since these elements correspond
-- to how what elements are kept from the original list, in those positions.
-- I.e., the first element of the result has every original element; the second
-- element in the result has every 2nd original element, etc.
skip :: [a] -> [[a]]
skip xs = map (\n -> keepNths n (zip indices xs)) indices
  where indices = scanl1 (+) (replicate (length xs) 1)

-- Exercise 2
-- Build a list of triples from the input list, centering each element around its
-- neighbours. Then, filter to only elements that are larger than both neighbours
-- and finally extract the middle elem of each tuple for the final result.
localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima l@(x:xs) = map (\y -> case y of (_, n, _) -> n)
                           (filter (\x -> case x of (a, b, c) -> (b > a && b > c)) (zip3 (x:l) l xs))

-- Exercise 3
-- The input list represents the histogram rows, in bottom-to-top order.
-- To add a new mark, search for a row that has that position unmarked. If there
-- is none, add a new row to the histogram; otherwise update the mark on the row.
-- NB: This is still explicitly recursive, should see if can eliminate that.
addMark :: Int-> [String] -> [String]
addMark n [] = [(replicate n ' ') ++ "*" ++ (replicate (9 - n) ' ')]
addMark n (x:xs) = case (x !! n) of
  ' ' -> ((take n x) ++ "*" ++ (drop (n+1) x)) : xs
  '*' -> x : (addMark n xs)

-- Convenient helper to add the histogram labels to the bottom of the graph.
addLabels :: [String] -> [String]
addLabels xs = "0123456789" : (replicate 10 '=') : xs

-- Fold over the input list, building up the histogram as a list of string (in
-- reverse order). Add labels, put in correct display order, then join the strings
-- together with newlines. The 'map fromIntegral' stuff is to make the types
-- work out for take, drop, !! in addMark, because they need an Int, not Integer.
histogram :: [Integer] -> String
histogram = concatMap (\x -> x ++ "\n") . reverse . addLabels . foldr addMark [] . map fromIntegral