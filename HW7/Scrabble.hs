{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where

newtype Score = Score Int
	deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i

instance Monoid Score where
	mempty  = Score 0
	mappend = (+)

score :: Char -> Score
score ch
	| ch == 'a' = 1
	| ch == 'b' = 3
	| ch == 'c' = 3
	| ch == 'd' = 2
	| ch == 'e' = 1
	| ch == 'f' = 4
	| ch == 'g' = 2
	| ch == 'h' = 4
	| ch == 'i' = 1
	| ch == 'j' = 8
	| ch == 'k' = 5
	| ch == 'l' = 1
	| ch == 'm' = 3
	| ch == 'n' = 1
	| ch == 'o' = 1
	| ch == 'p' = 3
	| ch == 'q' = 10
	| ch == 'r' = 1
	| ch == 's' = 1
	| ch == 't' = 1
	| ch == 'u' = 1
	| ch == 'v' = 4
	| ch == 'w' = 4
	| ch == 'x' = 8
	| ch == 'y' = 4
	| ch == 'z' = 10
	| otherwise = 0


scoreString :: [Char] -> Score
scoreString str = sum $ map score str