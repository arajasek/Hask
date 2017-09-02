{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
import Data.Monoid
import Sized
import Scrabble
import Buffer
import Editor

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
dropJ i (Append _ l1 l2)
 | i < intTag l1 = (dropJ i l1) +++ l2
 | otherwise = dropJ (i - intTag l1) l2

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ i l 
 | i <= 0 = Empty
 | i >= intTag l = l
takeJ i (Append _ l1 l2)
 | i <= intTag l1 = takeJ i l1
 | otherwise = l1 +++ takeJ (i - intTag l1) l2

--3

scoreLine :: String -> JoinList Score String
scoreLine l = Single (scoreString l) l

-- 4

instance Buffer (JoinList (Score, Size) String) where
 toString Empty = ""
 toString (Single _ a) = a
 toString (Append _ l r) = (toString l) ++ (toString r)

 fromString = foldr (+++) Empty . 
                map (\l -> Single ((scoreString l), (Size 1)) l) . lines

 line = indexJ

 replaceLine n ln buf = takeJ n buf +++ fromString ln +++ dropJ (n+1) buf

 numLines = intTag

 value Empty = 0
 value (Single ((Score i), _) _) = i
 value (Append ((Score i), _) _ _) = i

fromString' :: String -> (JoinList (Score, Size) String)
fromString' = fromString

main = runEditor editor $ fromString' $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ] 





