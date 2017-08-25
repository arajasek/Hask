{-# LANGUAGE FlexibleInstances #-} -- thanks to comments here: http://book.realworldhaskell.org/read/using-typeclasses.html

import Data.Maybe
import ExprT
import Parser
import StackVM

-- Exercise 1
eval :: ExprT -> Integer
eval (ExprT.Lit n) = n
eval (ExprT.Add e1 e2) = (eval e1) + (eval e2)
eval (ExprT.Mul e1 e2) = (eval e1) * (eval e2)

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr s = case parseExp ExprT.Lit ExprT.Add ExprT.Mul s of
  Just e -> Just (eval e)
  Nothing -> Nothing

-- Exercise 3
class Expr a where
 lit :: Integer -> a
 add :: a -> a -> a
 mul :: a -> a -> a

instance Expr ExprT where
  lit n = ExprT.Lit n
  add e1 e2 = ExprT.Add e1 e2
  mul e1 e2 = ExprT.Mul e1 e2

-- Exercise 4
instance Expr Integer where
  lit n = n
  add e1 e2 = e1 + e2
  mul e1 e2 = e1 * e2

instance Expr Bool where
  lit n = n > 0
  add e1 e2 = e1 || e2
  mul e1 e2 = e1 && e2

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
  lit n = MinMax n
  add (MinMax e1) (MinMax e2) = MinMax (max e1 e2)
  mul (MinMax e1) (MinMax e2) = MinMax (min e1 e2)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
  lit n = Mod7 (n `rem` 7)
  add (Mod7 e1) (Mod7 e2) = Mod7 ((e1 + e2) `rem` 7)
  mul (Mod7 e1) (Mod7 e2) = Mod7 ((e1 * e2) `rem` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger = testExp :: Maybe Integer -- -7
testBool = testExp :: Maybe Bool       -- True
testMM = testExp :: Maybe MinMax       -- 5
testSat = testExp :: Maybe Mod7        -- 0

-- Exercise 5
instance Expr Program where
  lit n = [StackVM.PushI n]
  add e1 e2 = e2 ++ e1 ++ [StackVM.Add]
  mul e1 e2 = e2 ++ e1 ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

-- Test
testProg = stackVM (fromJust (compile "2+3*4")) -- Right (IVal 14)