import Text.ParserCombinators.Parsec
import System.IO


csvFile = line `endBy` eol
line = cell `sepBy` (char ',')
cell = many (noneOf ",\n\r")
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "" input

main = do 
    file <- readFile "src/addresses.csv"
    case parseCSV file of
        Left x -> print x
        Right results -> print results