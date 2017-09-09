{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char

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
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
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

--1

first :: (a -> b) -> (a,c) -> (b,c)
first f (p,q) = (f p, q)

instance Functor Parser where
 fmap f (Parser p) = Parser (fmap (first f) . p)

--2

instance Applicative Parser where
  pure p = Parser (\l -> Just (p, l))
  (Parser p1) <*> (Parser p2) = Parser (\inp -> 
    case (p1 inp) of
      Nothing -> Nothing
      Just (a, s) -> case (p2 s) of
        Nothing -> Nothing
        Just (b, s2) -> Just (a b, s2)
    )

--3

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

kill _  = ()

abParser_ :: Parser ()
abParser_ = kill <$> abParser

negInt :: Parser Integer
negInt = (\ _ x -> 0 - x) <$> char '-' <*> posInt

anyInt :: Parser Integer
anyInt = posInt <|> negInt

intPair :: Parser [Integer]
intPair = (\x _ z -> [x,z]) <$> anyInt <*> char ' ' <*> anyInt

--4

instance Alternative Parser where
  empty = Parser (\_ -> Nothing)
  (Parser p1) <|> (Parser p2) = Parser (\inp -> 
    case (p1 inp) of
      Nothing -> (p2 inp)
      otherwise -> (p1 inp)
    )


--5

intOrUppercase :: Parser ()
intOrUppercase = kill <$> anyInt <|> kill <$> (satisfy isUpper) 



