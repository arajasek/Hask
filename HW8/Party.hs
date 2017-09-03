{-# OPTIONS_GHC -fno-warn-orphans #-}
import Employee
import Data.Monoid
import Data.Tree
import Data.List

--1

glCons :: Employee -> GuestList -> GuestList
glCons e (GL l f) = GL (e:l) (f + empFun e)

instance Monoid GuestList where
 mempty = GL [] 0
 mappend (GL l1 f1) (GL l2 f2) = GL (l1 ++ l2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun l m = max l m

 --2

treeFoldHelper :: (b -> a -> b) -> b -> [Tree a] -> b
treeFoldHelper f val [] = val
treeFoldHelper f val (p:l) = treeFoldHelper f (treeFold f val p) l

treeFold :: (b -> a -> b) -> b -> Tree a -> b
treeFold f val (Node n l) = f (treeFoldHelper f val l) n

--3 

mergeThem' :: [(GuestList, GuestList)] -> GuestList
mergeThem' l = foldr (\lis m -> (snd lis) <> m) mempty l

mergeThem :: [(GuestList, GuestList)] -> GuestList
mergeThem l = foldr (\lis m -> ((uncurry moreFun) lis) <> m) mempty l

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e l = ((glCons e (mergeThem' l)), (mergeThem l))

-- 4

fourFold :: (a -> [b] -> b) -> Tree a -> b
fourFold f (Node n ls) = f n ((map (fourFold f)) ls)

maxFun :: Tree Employee -> GuestList
maxFun = (uncurry moreFun) . (fourFold nextLevel)

--5

gLtoString :: GuestList -> String 
gLtoString (GL l f) = "Total fun:\n" ++ (show f) ++ "\n" ++ foldr (++) [] (sort (map ((++ "\n") . empName) l))

main :: IO () 
main = readFile "company.txt" >>= putStrLn . gLtoString . maxFun . read 




