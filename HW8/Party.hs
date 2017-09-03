module Party where
import Employee
import Data.Tree
import Data.Monoid
import Data.List

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL emps fun) = GL (emp:emps) (fun + empFun emp)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL xs nx) (GL ys ny) = GL (xs ++ ys) (nx + ny)

moreFun :: GuestList -> GuestList -> GuestList
moreFun a b
  | a > b     = a
  | otherwise = b

treeFold :: (a -> b -> b) -> b -> Tree a -> b
treeFold f init tree = case subForest tree of
                         [] -> newInit
			 _  -> foldr f' newInit (subForest tree)
			 where newInit = f (rootLabel tree) init 
			       f' = \t i -> treeFold f i t

treeFold1 :: (a -> [b] -> b) -> [b] -> Tree a -> b
treeFold1 f init tree = f (rootLabel tree) (map (treeFold1 f init) (subForest tree))

-- Exercise 3

-- We only use the new list if it beats the direct reports
augmentGL:: Employee -> (GuestList, GuestList) -> (GuestList, GuestList)
augmentGL emp (w, wo) = (max new w, w)
                        where new = glCons emp wo

maxFirst :: (GuestList, GuestList) -> (GuestList, GuestList)
maxFirst (a, b)
  | b > a     = (b, a)
  | otherwise = (a, b)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel emp l = augmentGL emp $ foldr (<>) (mempty, mempty) l

-- Exercise 4

maxFun :: Tree Employee -> GuestList
maxFun = (uncurry moreFun) . (treeFold1 nextLevel [(GL [] 0, GL [] 0)])

-- Exercise 5

formatGL :: GuestList -> String
formatGL (GL xs fun) = "Total fun: " ++ (show fun) ++ "\n" ++ (foldr (++) "" (sort $ map ((++ "\n") . empName) xs))

processFile :: String -> IO ()
processFile = putStrLn . formatGL . maxFun . read

main = readFile "company.txt" >>= processFile
