{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinList where

import Buffer
import Data.Monoid
import Editor
import Scrabble
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- Exercise 1
tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m l r) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty l = l
(+++) l Empty = l
(+++) left right = Append (tag left <> tag right) left right

-- Exercise 2
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ 0 (Single _ e) = Just e
indexJ i (Append _ l r) | i < tagL = indexJ i l
                        | otherwise = indexJ (i - tagL) r
  where tagL = getSize (size (tag l))
indexJ _ _ = Nothing

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ 0 l = l
dropJ n (Append _ l r) | n <= tagL = (dropJ n l) +++ r
                       | otherwise = dropJ (n - tagL) r
  where tagL = getSize (size (tag l))
dropJ _ _ = Empty

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ 0 l = Empty
takeJ n jl@(Append m l r) | n >= getSize (size m) = jl
                          | n <= tagL = takeJ n l
                          | otherwise = l +++ (takeJ (n - tagL) r)
  where tagL = getSize (size (tag l))
takeJ _ _ = Empty

-- for testing
x :: JoinList Size Char
x = (Single (Size 1) 'a') +++ (Single (Size 1) 'b') +++ (Single (Size 1) 'c') +++ (Single (Size 1) 'd')

-- Exercise 3
scoreLine:: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

-- Exercise 4
instance Buffer (JoinList (Score, Size) String) where
  toString Empty = ""
  toString (Single _ s) = s
  toString (Append _ l r) = toString l ++ toString r

  fromString = buildJL . lines

  line i b = indexJ i b

  replaceLine _ _ Empty = Empty
  replaceLine 0 ln (Single _ _) = (Single (scoreString ln, 1) ln)
  replaceLine i ln (Append _ l r)
    | i < lsize = (replaceLine i ln l) +++ r
    | otherwise = l +++ (replaceLine (i - lsize) ln r)
    where lsize = getSize . snd $ tag l
  replaceLine _ _ b = b

  numLines = getSize . snd . tag
  value = getScore . fst . tag

buildJL :: [String] -> JoinList (Score, Size) String
buildJL [] = Empty
buildJL [x] = Single (scoreString x, 1) x
buildJL xs = (buildJL $ take n xs) +++ (buildJL $ drop n xs)
  where n = (length xs) `div` 2

buffer :: JoinList (Score, Size) String
buffer = fromString $ unlines [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]

main = runEditor editor buffer