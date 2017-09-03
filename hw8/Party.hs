import Employee
import Data.Tree

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL ls fun) = GL (emp : ls) ((empFun emp) + fun)


moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2
	| gl1 > gl2 = gl1
	| otherwise = gl2


instance Monoid GuestList where
	mempty  = GL [] 0
	mappend = moreFun


-- I think I can take out the accumulator?
treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold fn acc (Node emp []) = fn emp []
treeFold fn acc (Node emp ls) = fn emp (map (treeFold fn acc) ls)


-- Impossible since we don't know whether we took the previous boss
-- combineGLs :: Employee -> [GuestList] -> GuestList
-- combineGLs emp ls = map (\gl -> moreFun (glCons emp gl) gl) ls 

-- (with, without)
nextLevelHelper :: Employee -> (GuestList, GuestList) -> (GuestList, GuestList)
nextLevelHelper emp (w, wo) = (glCons emp wo, w `mappend` wo)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel emp ls = (mconcat (map fst (map (nextLevelHelper emp) ls)), mconcat (map snd (map (nextLevelHelper emp) ls)))


maxPair :: (GuestList, GuestList) -> GuestList
maxPair (gl1, gl2) = gl1 `mappend` gl2

maxFun :: Tree Employee -> GuestList
maxFun te = maxPair (treeFold nextLevel (GL [] 0, GL [] 0) te)



-- String -> IO ()
stupid :: String -> IO ()
stupid _ = putStrLn "test"

main :: IO ()
main = (readFile "company.txt") >>= stupid