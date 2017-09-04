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
-- Exercise 1
-- Found it easier to change the type signature a bit for `first`.
first :: (a -> b) -> Maybe (a, c) -> Maybe (b, c)
first f (Just (x, y)) = Just (f x, y)
first _ Nothing = Nothing

instance Functor Parser where
  fmap g (Parser {runParser = f}) = Parser ((first g) . f)

-- The above obeys the functor laws, because if g = id, then (first g) = id,
-- so ((first g) . f) = f, so fmap id = id.

-- Exercise 2
instance Applicative Parser where
  pure x = Parser (\input -> Just (x, input)) -- So, we don't consume any of the input, we just
                                              -- pass it back, unparsed, with the 'injected' val.

  -- The description in the assignment for this part sounds almost verbatim like the description
  -- in the Typeclassopedia for the IO instance of Applicative, but the source code is not too
  -- helpful? https://hackage.haskell.org/package/base-4.10.0.0/docs/src/GHC.Base.html#%3C%2A%3E
  (Parser {runParser = p1}) <*> (Parser {runParser = p2})
    = p1 >>=