import Data.Monoid
import Sized

data JoinList m a = Empty
 | Single m a
 | Append m (JoinList m a) (JoinList m a)
 deriving (Eq, Show)

--1

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) l1 l2 = Append (tag l1 <> tag l2) l1 l2


tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m a) = m
tag (Append m _ _) = m

--2

intTag :: (Sized m, Monoid m) => JoinList m a -> Int
intTag = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i _ | i < 0 = Nothing 
indexJ 0 (Single _ a) = Just a
indexJ i (Append m l1 l2) 
 | i < (intTag l1) = indexJ i l1
 | otherwise = indexJ (i - (intTag l1)) l2
indexJ _ _ = Nothing

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ i l 
 | i <= 0 = l
 | i >= intTag l = Empty
dropJ i l@(Append m l1 l2)
 | i <= intTag l1 = Append (((intTag l) - i) <> mempty) (dropJ i l1) l2
 | otherwise = Append (((intTag l) - i) <> mempty) Empty (dropJ (i - intTag l1) l2)
