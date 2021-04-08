import Text.ParserCombinators.Parsec
import System.IO
csvFile = endBy line eol
line = sepBy cell (char ',')
cell = many (noneOf ",\n")
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "" input

main = do 
    handle <- openFile "src/addresseseasy.csv" ReadMode 
    contents <- hGetContents handle
    case parseCSV contents of
        Left x -> print x
        Right results -> print results
    hClose handle


