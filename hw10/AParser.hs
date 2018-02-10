{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import Control.Applicative

import Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
    where
    f [] = Nothing      -- fail on the empty input
    f (x:xs)            -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
        | null ns   = Nothing
        | otherwise = Just (read ns, rest)
        where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

first :: (a -> b) -> (a,c) -> (b,c)
first fn (f,s) = (fn f, s)

-- (a -> b) -> Parser a -> Parser b
-- runParser returns Maybe (a,String)
instance Functor Parser where
    fmap fn p = Parser {runParser = fmap (first fn) . (runParser p)}


-- Can't be shortened.
-- fmap will always return something in a maybe context, so if the original 
-- function is also indeterministic (returns something in a maybe context) we
-- would have nested maybes.

runContinuation :: Parser a -> Maybe (a->b, String) -> Maybe (b, String)
runContinuation _ Nothing = Nothing
runContinuation p (Just (k, rest)) = (runParser (fmap k p)) rest


instance Applicative Parser where
 pure a = Parser {runParser = \input -> Just(a,input)}
 p1 <*> p2 = Parser { runParser = (runContinuation p2) . (runParser p1)}

abParser :: Parser (Char, Char)
abParser = liftA2 (,) (char 'a') (char 'b')

abParser_ :: Parser ()
abParser_ = (\_ _ -> ()) <$> char 'a' <*> char 'b'

intPair :: Parser [Integer]
intPair = (\first _ second -> [first, second]) <$> posInt <*> char ' ' <*> posInt

instance Alternative Parser where
    empty = Parser {runParser = \_ -> Nothing}
    p1 <|> p2 = Parser {runParser = \input -> ((runParser p1) input) <|> ((runParser p2) input)}

intOrUppercase :: Parser ()
intOrUppercase = ((\_ -> ()) <$> posInt) <|> ((\_ -> ()) <$> (satisfy isUpper))