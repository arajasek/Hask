{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scrabble where
import Data.Monoid
import Data.Char

newtype Score = Score Int
  deriving (Show, Eq, Num, Ord)

instance Monoid Score where
  mempty = Score 0
  mappend = (+)

score :: Char -> Score
score char
  | elem c "aeioutnrsl" = Score 1
  | elem c "dg"         = Score 2
  | elem c "cmbp"       = Score 3
  | elem c "hfwyv"      = Score 4
  | c == 'k'            = Score 5
  | elem c "jx"         = Score 8
  | elem c "qz"         = Score 10
  | otherwise           = Score 0
  where c = toLower char

scoreString :: String -> Score
scoreString = foldr (<>) mempty . map score

toInt :: Score -> Int
toInt (Score n) = n
