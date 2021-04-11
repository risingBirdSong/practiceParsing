-- file: ch16/csv2.hs
import Text.ParserCombinators.Parsec
import Data.List
import Data.Char
import Debug.Trace
-- import Control.Monad


csvFile =  try (endBy line eol) <|> try (spaces >> (return [])) <|> (eof >> return [])
line = sepBy cell (char ',')
cell = many (noneOf ",\n\r")

-- eol =   try (string "\n\r")
--     <|> try (string "\r\n")
--     <|> string "\n"
--     <|> string "\r"

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
trim = dropWhileEnd isSpace . dropWhile isSpace


parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse (csvFile) "(unknown)" input

parseCSV' = do 
    file <- readFile "src/clean.csv"
    let cleaned = (trim file) ++ "\n"
    print file
    let results = parseCSV file 
    case results of
        (Right v) -> print v 
        (Left e) -> print e

-- *Main> parseCSV'
-- "1,hello,brownie\n2,goodbye,pickle    "
-- [["1","hello","brownie"],["2","goodbye","pickle"]]