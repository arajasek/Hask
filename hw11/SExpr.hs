{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = liftA2 (:) p (zeroOrMore p) <|> pure [] -- use alternative to change apply's 'base case' from
                                                       -- Nothing -> Nothing, to Nothing -> []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = liftA2 (:) p (zeroOrMore p) -- if initial p fails, will get Nothing

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = liftA2 (++) (oneOrMore (satisfy isAlpha)) (zeroOrMore (satisfy isAlphaNum))

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

atom :: Parser Atom
atom = liftA N posInt <|> liftA I ident

parseSExpr :: Parser SExpr
parseSExpr = spaces *> (liftA A atom <|> ((char '(') *> liftA Comb (oneOrMore parseSExpr) <* (char ')'))) <* spaces