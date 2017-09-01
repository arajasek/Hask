{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module JoinList where
import Data.Monoid
import Sized
import Scrabble
import Buffer
import Editor

data JoinList m a = Empty
                  | Single m a
		  | Append m (JoinList m a) (JoinList m a)
  deriving (Show, Eq)

-- Exercise 1
tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a <> tag b) a b

-- Exercise 2
indexJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ 0 (Single _ a) = Just a
indexJ _ (Single _ _) = Nothing
indexJ n (Append s left right)
  | n < 0          = Nothing
  | n >= totalSize = Nothing
  | n >= leftSize  = indexJ (n - leftSize) right
  | otherwise      = indexJ n left
  where leftSize = getSize $ size $ tag left
        totalSize = getSize $ size s

dropJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n (Single m a)
  | n <= 0    = Single m a
  | otherwise = Empty
dropJ n (Append s left right)
  | n <= 0         = Append s left right
  | n >= totalSize = Empty
  | n >= leftSize  = dropJ (n - leftSize) right
  | otherwise      = (dropJ n left) +++ right
  where leftSize = getSize $ size $ tag left
        totalSize = getSize $ size s

takeJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n (Single m a)
  | n <= 0    = Empty
  | otherwise = Single m a
takeJ n (Append s left right)
  | n <= 0         = Empty
  | n >= totalSize = Append s left right
  | n >= leftSize  = left +++ (takeJ (n - leftSize) right)
  | otherwise      = takeJ n left
  where leftSize = getSize $ size $ tag left
        totalSize = getSize $ size s

a = Single (Size 1) 'a'
b = Single (Size 1) 'b'
c = Single (Size 1) 'c'

testJoinList = (a +++ b) +++ c

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

scoreAndSizeLine :: String -> JoinList (Score, Size) String
scoreAndSizeLine s = Single (scoreString s, 1) s

instance Buffer (JoinList (Score, Size) String) where
  toString Empty = []
  toString (Single _ s) = s
  toString (Append _ left right) = toString left ++ toString right

  fromString = foldr (+++) Empty . map scoreAndSizeLine . lines

  line = indexJ

  replaceLine n string buffer
    | n >= bufferSize = buffer
    | n < 0           = buffer
    | otherwise       = takeJ n buffer +++ newLine +++ dropJ (n + 1) buffer
    where newLine = scoreAndSizeLine string
          bufferSize = getSize $ size $ tag buffer

  numLines = getSize . size . tag

  value = toInt . fst . tag

fromString' :: String -> JoinList (Score, Size) String
fromString' = fromString

main = runEditor editor $ fromString' $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]
