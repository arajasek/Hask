{-# LANGUAGE FlexibleInstances #-}
import ExprT
import Parser 
import StackVM

eval :: ExprT -> Integer
eval (Lit t) = t
eval (ExprT.Add a b) = (eval a) + (eval b)
eval (ExprT.Mul a b) = (eval a) * (eval b)

evalStr :: String -> Maybe Integer
evalStr s = case (parseExp ExprT.Lit ExprT.Add ExprT.Mul s) of
 Just t -> Just (eval t)
 Nothing -> Nothing

class Expr a where
 lit :: Integer -> a
 mul :: a -> a -> a
 add :: a -> a -> a

instance Expr ExprT where
 lit n  = (ExprT.Lit n)
 mul a b = (ExprT.Mul a b)
 add a b = (ExprT.Add a b)

instance Expr Integer where
 lit n  = n
 mul a b = a * b
 add a b = a + b

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Bool where
 lit n  = (n > 0)
 mul a b = (a && b)
 add a b = (a || b)

instance Expr MinMax where
 lit n  = MinMax n
 mul (MinMax a) (MinMax b)
  | (a < b) = MinMax a
  | otherwise = MinMax b
 add (MinMax a) (MinMax b)
  | (a < b) = MinMax b
  | otherwise = MinMax a

instance Expr Mod7 where
 lit n  = Mod7 n
 mul (Mod7 a) (Mod7 b) = Mod7 (mod (a*b) 7)
 add (Mod7 a) (Mod7 b) = Mod7 (mod (a+b) 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

instance Expr Program where
 lit n  = [PushI n]
 mul a b = a ++ b ++ [StackVM.Mul]
 add a b = a ++ b ++ [StackVM.Add]

compile :: String -> Maybe Program
compile = (parseExp lit add mul) 