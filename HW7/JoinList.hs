module JoinList where

import Sized
import Scrabble

data JoinList m a = Empty
	| Single m a
	| Append m (JoinList m a) (JoinList m a)
	deriving (Eq, Show)


tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m a) = m
tag (Append m _ _) = m


(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) left right = Append ((tag left) `mappend` (tag right)) left right


jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l r) = (jlToList l) ++ (jlToList r)


indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single _ element)
	| i == 0 = Just element
	| otherwise = Nothing
indexJ i (Append m left right)
	| i >= (getSize (size m)) = Nothing
	| i < 0 = Nothing
	| i < (getSize (size (tag left))) = (indexJ i left)
	| otherwise = (indexJ (i - (getSize (size (tag left)))) right)


dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n orig@(Single _ _)
	| n <= 0 = orig
	| otherwise = Empty
dropJ n orig@(Append m left right)
	| n <= 0 = orig
	| n < (getSize (size (tag left))) = (dropJ n left) +++ right
	| otherwise = (dropJ (n - (getSize (size (tag left)))) right)


-- takeJ n Empty not working
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n orig@(Single _ _)
	| n <= 0 = Empty
	| otherwise = orig
takeJ n orig@(Append m left right)
	| n <= 0 = Empty
	| n <= (getSize (size (tag left))) = (takeJ n left)
	| otherwise = left +++ (takeJ (n - (getSize (size (tag left)))) right)

scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str