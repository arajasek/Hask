{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where

import Data.Monoid

-- Exercise 3
newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

instance Monoid Score where
  mempty = Score 0
  mappend = (+)

score :: Char -> Score
score 'k' = 5 -- only one char w/ score = 5?
score c | c `elem` "aeilnorstu" = 1
        | c `elem` "dg"         = 2
        | c `elem` "bcmp"       = 3
        | c `elem` "fhvwy"      = 4
        | c `elem` "jx"         = 8
        | c `elem` "qz"         = 10
score _ = 0

scoreString :: String -> Score
scoreString = mconcat . map score

getScore :: Score -> Int
getScore (Score n) = n