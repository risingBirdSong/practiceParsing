import Text.ParserCombinators.Parsec

csvFile = endBy line eol
line = sepBy cell (char ',')
cell = quotedCell <|> many (noneOf ",\n\r")

quotedCell = 
    do char '"'
       content <- many quotedChar
       char '"' <?> "quote at end of cell"
       return content

quotedChar =
        noneOf "\""
    <|> try (string "\"\"" >> return '"')

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

main =
    do c <- getContents
       case parse csvFile "(stdin)" c of
            Left e -> do putStrLn "Error parsing input:"
                         print e
            Right r -> mapM_ print r


parseCSV' = do 
    file <- readFile "src/addresses.csv"
    print file 
    let results = parseCSV (file ++ "\n") 
    case results of 
        (Right val) -> print val 
        (Left e) -> print e 


-- [["John","Doe","120 jefferson st.","Riverside"," NJ"," 08075"],
--  ["Jack","McGinnis","220 hobo Av.","Phila"," PA","09119"],
--  ["John \"Da Man\"","Repici","120 Jefferson St.","Riverside"," NJ","08075"],
--  ["Stephen","Tyler","7452 Terrace \"At the Plaza\" road","SomeTown","SD"," 91234"],
--  ["","Blankman","","SomeTown"," SD"," 00298"],["Joan \"the bone\", Anne","Jet","9th, at Terrace plc","Desert City","CO","00123"],["harold"," mcdermont"," 123 cherry lane."," Pasco"," WA"," 99301"],[""]]