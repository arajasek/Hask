import Employee
import Data.Tree
import Data.Monoid
import Data.List

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL ls fun) = GL (emp : ls) ((empFun emp) + fun)


moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2
	| gl1 > gl2 = gl1
	| otherwise = gl2


instance Monoid GuestList where
	mempty  = GL [] 0
	mappend = \(GL ls1 f1) (GL ls2 f2) -> GL (ls1 ++ ls2) (f1 + f2)


-- I think I can take out the accumulator?
treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold fn acc (Node emp []) = fn emp []
treeFold fn acc (Node emp ls) = fn emp (map (treeFold fn acc) ls)


-- (with, without)
maxPair :: (GuestList, GuestList) -> GuestList
maxPair (gl1, gl2) = moreFun gl1 gl2

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel emp ls = (glCons emp (mconcat (map snd ls)), mconcat (map maxPair ls))

maxFun :: Tree Employee -> GuestList
maxFun te = maxPair (treeFold nextLevel (GL [] 0, GL [] 0) te)


guestlistNumber :: GuestList -> String
guestlistNumber (GL _ fun) = "Total fun: " ++ (show fun)

guestlistNames :: [String] -> String
guestlistNames names = foldr (\name acc -> name ++ "\n" ++ acc) "" names

sortNames :: GuestList -> [String]
sortNames (GL ls _) = sort (map (\emp -> (empName emp)) ls)

glToString :: GuestList -> String
glToString gl = (guestlistNumber gl) ++ "\n" ++ (guestlistNames (sortNames gl))

process :: String -> IO ()
process inputStr = putStrLn (glToString (maxFun (read inputStr)))

main :: IO ()
main = (readFile "company.txt") >>= process