import Log
import Text.Read

parseMessage1 :: [String] -> LogMessage
parseMessage1 [] = Unknown
parseMessage1 [n] = Unknown n
parseMessage1 


parseMessage :: String -> LogMessage
parseMessage = parseMessage1 . words
