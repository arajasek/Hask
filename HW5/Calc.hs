module Calc where
import ExprT
import Parser

-- Exercise 1
eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add a b) = (eval a) + (eval b)
eval (Mul a b) = (eval a) * (eval b)

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit Add Mul s of
              (Nothing) -> Nothing
	      (Just t) -> Just (eval t)

-- Exercise 3
class Expr a where
  lit :: a -> Expr a
  add :: Expr a -> Expr a -> Expr a
  mul :: Expr a -> Expr a -> Expr a

-- Exercise 4
instance Expr Integer where
  lit n = Lit n
  add a b = Add a b
  mul a b = Mul a b


