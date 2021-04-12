-- file: ch16/csv6.hs
import Text.ParserCombinators.Parsec 
import qualified Text.Parsec.Prim as Prim 
import qualified Data.Functor.Identity as Id 
import Control.Monad

csvFile = manyTill line eof 
-- csvFile = endBy line eol
line = sepBy cell (char ',')
-- cell :: Text.Parsec.Prim.ParsecT [Char] u Data.Functor.Identity.Identity [Char]
cell :: Prim.ParsecT [Char] u Id.Identity [Char]
cell = many (noneOf ",\n\r")
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"

escapeCell :: Prim.ParsecT [Char] u Id.Identity [Char]
escapeCell = do 
    void $ char '\"'
    thecell <- many (noneOf ",\n\r\"")
    void $ char '\"'
    return thecell

parseCSV :: String -> Either Text.ParserCombinators.Parsec.ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input


parseCSV' = do 
    file <- readFile "src/addresses.csv"
    print file 
    let results = parseCSV (file ++ "\n") 
    case results of 
        (Right val) -> print val 
        (Left e) -> print e 


-- parse (eol >> eof) "" "\n\r"