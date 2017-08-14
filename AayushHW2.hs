module LogAnalysis where 
import Log
import Text.Read
import Control.Applicative

makeErr2 :: String -> String -> Maybe Int -> [String] -> LogMessage
makeErr2 n2 n3 Nothing l = Unknown ("E "++n2++" "++n3++" "++(unwords l))
makeErr2 n2 n3 (Just a) l = (LogMessage (Error a) (read n2) (unwords l))

makeErr :: 	String -> [String] -> LogMessage
makeErr n2 [] = Unknown ("E "++n2)
makeErr n2 (n3:l) = makeErr2 n2 n3 (readMaybe n3) l

makeMessage :: String -> String ->[String] -> LogMessage
makeMessage n1 n2 l
 | n1 == "E" = (makeErr n2 l)
 | n1 == "I" =  (LogMessage Info (read  n2) (unwords l))
 | n1 == "W" = (LogMessage Warning (read n2) (unwords l))
 | otherwise = Unknown (n1++n2++(unwords l))

parseMessage2:: String -> String -> Maybe Int -> [String] -> LogMessage
parseMessage2 n1 n2 Nothing l = Unknown (n1++" "++n2++" "++(unwords l))
parseMessage2 n1 n2 (Just a) l = (makeMessage n1 n2 l)

parseMessage1 :: [String] -> LogMessage
parseMessage1 [] = Unknown []
parseMessage1 ([n]) = Unknown n
parseMessage1 (n1:(n2:l)) = (parseMessage2 n1 n2 (readMaybe n2) l)

parseMessage :: String -> LogMessage
parseMessage s = parseMessage1 (words s)

parse2 :: [String] -> [LogMessage]
parse2 [] = []
parse2 (n:l) = (parseMessage n) : (parse2 l)

parse :: String -> [LogMessage]
parse l = parse2 (lines l)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) n = n
insert lm Leaf = (Node 	Leaf lm Leaf)
insert (LogMessage mt ts str) (Node l (LogMessage curmt curts curstr) r)
 | ts <= curts = (Node (insert (LogMessage mt ts str) l) (LogMessage curmt curts curstr) r)
 | otherwise = (Node l (LogMessage curmt curts curstr) (insert (LogMessage mt ts str) r))

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (n:l) = (insert n (build l))

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l lm r) = (inOrder l) ++ [lm] ++ (inOrder r)

whatWentWrongSorted :: [LogMessage] -> [String]
whatWentWrongSorted [] = []
whatWentWrongSorted ((LogMessage (Error n) _ str):l) 
 | n >= 50 = str:(whatWentWrongSorted l)
 | otherwise = (whatWentWrongSorted l)
whatWentWrongSorted (_:l) = whatWentWrongSorted l

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong t = (whatWentWrongSorted (inOrder (build t)))

main = putStrLn (parse <$> readFile "sample.log")