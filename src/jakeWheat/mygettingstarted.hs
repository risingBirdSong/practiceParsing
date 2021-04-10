
{-

Wrappers for the Text.Parsec.Char module with the types fixed to
'Text.Parsec.String.Parser a', i.e. the stream is String, no user
state, Identity monad.

-}

-- following along to https://jakewheat.github.io/intro_to_parsing/#getting-started


import Text.Parsec.String (Parser)
import qualified Text.Parsec.Char as Chr
import qualified Text.Parsec.Combinator as Comb
import qualified Text.Parsec.Expr as E
import qualified Text.Parsec as P

import Data.Char

import Text.Parsec (ParseError)
import Control.Applicative ((<$>), (<*>), (<*), (*>), many)
import Control.Monad (void)



spaces :: Parser ()
spaces = Chr.spaces

space :: Parser Char
space = Chr.space

newline :: Parser Char
newline = Chr.newline

tab :: Parser Char
tab = Chr.tab

upper :: Parser Char
upper = Chr.upper

lower :: Parser Char
lower = Chr.lower

alphaNum :: Parser Char
alphaNum = Chr.alphaNum

letter :: Parser Char
letter = Chr.letter

digit :: Parser Char
digit = Chr.digit

hexDigit :: Parser Char
hexDigit = Chr.hexDigit

octDigit :: Parser Char
octDigit = Chr.octDigit

char :: Char -> Parser Char
char = Chr.char

string :: String -> Parser String
string = Chr.string

anyChar :: Parser Char
anyChar = Chr.anyChar

oneOf :: [Char] -> Parser Char
oneOf = Chr.oneOf

noneOf :: [Char] -> Parser Char
noneOf = Chr.noneOf

satisfy :: (Char -> Bool) -> Parser Char
satisfy = Chr.satisfy




{-

Wrappers for the Text.Parsec.Combinator module with the types fixed to
'Text.Parsec.String.Parser a', i.e. the stream is String, no user
state, Identity monad.

-}









choice :: [Parser a] -> Parser a
choice = Comb.choice


count :: Int -> Parser a -> Parser [a]
count = Comb.count

between :: Parser open -> Parser close -> Parser a -> Parser a
between = Comb.between


option :: a -> Parser a -> Parser a
option = Comb.option

optionMaybe :: Parser a -> Parser (Maybe a)
optionMaybe = Comb.optionMaybe

optional :: Parser a -> Parser ()
optional = Comb.optional

skipMany1 :: Parser a -> Parser ()
skipMany1 = Comb.skipMany1

many1 :: Parser a -> Parser [a]
many1 = Comb.many1

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy = Comb.sepBy

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 = Comb.sepBy1

endBy :: Parser a -> Parser sep -> Parser [a]
endBy = Comb.endBy

endBy1 :: Parser a -> Parser sep -> Parser [a]
endBy1 = Comb.endBy1

sepEndBy :: Parser a -> Parser sep -> Parser [a]
sepEndBy = Comb.sepEndBy

sepEndBy1 :: Parser a -> Parser sep -> Parser [a]
sepEndBy1 = Comb.sepEndBy1

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl = Comb.chainl

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 = Comb.chainl1

chainr :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr = Comb.chainr

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 = Comb.chainr1

eof :: Parser ()
eof = Comb.eof

notFollowedBy :: Show a => Parser a -> Parser ()
notFollowedBy = Comb.notFollowedBy

manyTill :: Parser a -> Parser end -> Parser [a]
manyTill = Comb.manyTill

lookAhead :: Parser a -> Parser a
lookAhead = Comb.lookAhead

anyToken :: Parser Char
anyToken = Comb.anyToken













-- not sure if this is neccessary, or a type alias would be good
-- enough
data Operator a = Infix (Parser (a -> a -> a)) E.Assoc
                | Prefix (Parser (a -> a))
                | Postfix (Parser (a -> a))

type OperatorTable a = [[Operator a]]

buildExpressionParser :: OperatorTable a -> Parser a -> Parser a
buildExpressionParser t = E.buildExpressionParser (map (map f) t)
  where
    f (Infix p a) = E.Infix p a
    f (Prefix p) = E.Prefix p
    f (Postfix p) = E.Postfix p








try :: Parser a -> Parser a
try = P.try

parse :: Parser a -> P.SourceName -> String -> Either P.ParseError a
parse = P.parse


-- functions-and-types-for-parsing

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""


parseWithLeftOver :: Parser a -> String -> Either ParseError (a,String)
parseWithLeftOver p = parse ((,) <$> p <*> leftOver) ""
   where leftOver = manyTill anyToken eof


num :: Parser Integer 
num = do 
  n <- many1 digit
  return (read n)


var :: Parser String
var = do
    fc <- firstChar
    rest <- many nonFirstChar
    return (fc:rest)
  where
    firstChar = satisfy (\a -> isLetter a || a == '_')
    nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')


data Parentheses = Parentheses Integer
                   deriving (Eq,Show)


parens :: Parser Parentheses
parens = do
    void $ char '('
    e <- many1 digit
    void $ char ')'
    return (Parentheses (read e))

-- parensWithWarning :: Parser Parentheses
-- parensWithWarning = do
--     char '('
--     e <- many1 digit
--     char ')'
--     return (Parentheses (read e))

data SingleAdd = SingleAdd Integer Integer
                 deriving (Eq,Show)

addP :: Parser SingleAdd
addP = do
    e0 <- many1 digit
    void $ char '+'
    e1 <- many1 digit
    return (SingleAdd (read e0) (read e1))

addP' :: Parser SingleAdd
addP' = do 
  a <- lexeme $ many1 digit 
  void $ lexeme $ char '+'
  b <- lexeme $ many1 digit
  return (SingleAdd (read a) (read b))


whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"


parensW :: Parser Parentheses
parensW = do
    whitespace
    void $ char '('
    whitespace
    e <- many1 digit
    whitespace
    void $ char ')'
    whitespace
    return (Parentheses (read e))


lexeme :: Parser a -> Parser a
lexeme p = do
           x <- p
           whitespace
           return x

parseWithWhitespace :: Parser a -> String -> Either ParseError a
parseWithWhitespace p = parseWithEof wrapper
  where
    wrapper = do
        whitespace
        p


parensL :: Parser Parentheses
parensL = do
    void $ lexeme $ char '('
    e <- lexeme $ many1 digit
    void $ lexeme $ char ')'
    return (Parentheses (read e))


parseWithWhitespace' :: Parser a -> String -> Either ParseError a
parseWithWhitespace' p = parseWithEof (whitespace >> p)



data SimpleExpr = Num Integer
                | Var String
                | Add SimpleExpr SimpleExpr
                | Parens SimpleExpr
                  deriving (Eq,Show)


numE :: Parser SimpleExpr
numE = do
    n <- lexeme $ many1 digit
    return $ Num $ read n


varE :: Parser SimpleExpr
varE = lexeme $ do
    fc <- firstChar
    rest <- many nonFirstChar
    return $ Var (fc:rest)
  where
    firstChar = satisfy (\a -> isLetter a || a == '_')
    nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')


parensE :: Parser SimpleExpr
parensE = do
    void $ lexeme $ char '('
    e <- lexeme $ many1 digit
    void $ lexeme $ char ')'
    return $ Parens $ Num $ read e


parensE' :: Parser SimpleExpr
parensE' = do
    void $ lexeme $ char '('
    e <- numE
    void $ lexeme $ char ')'
    return $ Parens e


addE :: Parser SimpleExpr
addE = do
    e0 <- numE
    void $ lexeme $ char '+'
    e1 <- numE
    return $ Add e0 e1

-- *Main> parseWithWhitespace addE "1+2"
-- Right (Add (Num 1) (Num 2))


addAST (Right (Add (Num a) (Num b))) = Num (a + b)


-- *Main> addAST (Right (Add (Num 1) (Num 2)))
-- Num 3


numOrVar :: Parser SimpleExpr
numOrVar = numE P.<|> varE


numOrVar' :: Parser SimpleExpr
numOrVar' = choice [numE,varE]


simpleExpr :: Parser SimpleExpr
simpleExpr = numE P.<|> varE P.<|> addE P.<|> parensE


simpleExpr2 :: Parser SimpleExpr
simpleExpr2 = try addE P.<|> numE P.<|> varE P.<|> parensE


parensE3 :: Parser SimpleExpr
parensE3 = do
    void $ lexeme $ char '('
    e <- simpleExpr3
    void $ lexeme $ char ')'
    return $ Parens e

addE3 :: Parser SimpleExpr
addE3 = do
    e0 <- simpleExpr3
    void $ lexeme $ char '+'
    e1 <- simpleExpr3
    return $ Add e0 e1



simpleExpr3 :: Parser SimpleExpr
simpleExpr3 = try addE3 P.<|> numE P.<|> varE P.<|> parensE3


-- the problem here is infinite recursion because addE3 and simpleExpr3 keep calling each other


parensE4 :: Parser SimpleExpr
parensE4 = do
    void $ lexeme $ char '('
    e <- simpleExpr4
    void $ lexeme $ char ')'
    return $ Parens e


simpleExpr4 :: Parser SimpleExpr
simpleExpr4 = numE P.<|> varE P.<|> parensE4


data ValidBraces = Num' Int | Parens' ValidBraces | List' ValidBraces | Curly' ValidBraces deriving (Show, Eq)


parensMatch :: Parser ValidBraces
parensMatch = do 
  void $ lexeme $ char '('
  e <- checkBalance
  void $ lexeme $ char ')'
  return $ Parens' e

listMatch :: Parser ValidBraces
listMatch = do 
  void $ lexeme $ char '['
  e <- checkBalance
  void $ lexeme $ char ']'
  return $ List' e

curlyMatch :: Parser ValidBraces
curlyMatch = do 
  void $ lexeme $ char '{'
  e <- checkBalance
  void $ lexeme $ char '}'
  return $ Curly' e

numMatch' :: Parser ValidBraces
numMatch' = do 
  dgt <- many1 digit
  return $ Num' (read dgt) 
 

checkBalance :: Parser ValidBraces
checkBalance = try parensMatch P.<|> listMatch P.<|> curlyMatch P.<|> numMatch'