{-# OPTIONS_GHC -fno-warn-orphans #-}
import Data.List
import Data.Monoid
import Data.Tree
import Employee

-- Exercise 1
glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp _ ef) (GL gl tf) = GL (e:gl) (tf+ef)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL gl1 f1) (GL gl2 f2) = GL (gl1 ++ gl2) (f1+f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max -- this works b/c they've defined compare for GuestLists

-- Exercise 2
treeFold ::  (a -> [b] -> b) -> b -> Tree a -> b
treeFold f base (Node r c) = f r (map (treeFold f base) c)

-- Exercise 3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e@(Emp _ f) [] = ((GL [e] f), (GL [] 0))
nextLevel e@(Emp _ f) gls = ((glCons e (mconcat (map snd gls))), (mconcat (map (\(x, y) -> moreFun x y) gls)))

-- Exercise 4
maxFun :: Tree Employee -> GuestList
maxFun t = case (treeFold nextLevel (mempty, mempty) t) of (x, y) -> moreFun x y

-- Exercise 5
sortedGL :: GuestList -> [String]
sortedGL (GL es f) = ("Total fun: " ++ (show f)) : (sortedNames es)

sortedNames :: [Employee] -> [String]
sortedNames = sort . (map empName)

main = readFile "company.txt" >>= (\s -> putStrLn (concatMap (\x -> x ++ "\n") (sortedGL (maxFun (read s :: (Tree Employee))))))