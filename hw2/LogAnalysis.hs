{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Exercise 1
parseMessageHelper :: [String] -> LogMessage
parseMessageHelper ("E":n:ts:msg) = LogMessage (Error (read n::Int)) (read ts::Int) (unwords msg)
parseMessageHelper ("I":ts:msg) = LogMessage Info (read ts::Int) (unwords msg)
parseMessageHelper ("W":ts:msg) = LogMessage Warning (read ts::Int) (unwords msg)
parseMessageHelper msg = Unknown (unwords msg)

parseMessage :: String -> LogMessage
parseMessage = parseMessageHelper . words -- point-free style, so fancy!

parseHelper :: [String] -> [LogMessage]
parseHelper [] = []
parseHelper (m:ms) = parseMessage m : parseHelper ms

parse :: String -> [LogMessage]
parse = parseHelper . lines

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mtree = mtree
insert msg@(LogMessage _ ts _) Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ ts _) (Node left root@(LogMessage _ rts _) right)
  | ts < rts = Node (insert msg left) root right
  | otherwise = Node left root (insert msg right)

-- for testing
mt1 = Node Leaf (LogMessage Info 100 "msg") Leaf
mt2 = Node mt1 (LogMessage (Error 22) 110 "msg2") (Node (Node Leaf (LogMessage Warning 115 "msg3") Leaf) (LogMessage Info 117 "msg4") Leaf)

-- Exercise 3
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (m:ms) = insert m (build ms)

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left root right) = (inOrder left) ++ [root] ++ (inOrder right)

-- Exercise 5
whatWentWrongFilter :: [LogMessage] -> [String]
whatWentWrongFilter [] = []
whatWentWrongFilter ((LogMessage (Error n) _ m):ms)
  | n >= 50 = m : whatWentWrongFilter ms
  | otherwise = whatWentWrongFilter ms
whatWentWrongFilter (_:ms) = whatWentWrongFilter ms

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = whatWentWrongFilter . inOrder . build