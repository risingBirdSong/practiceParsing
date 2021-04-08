
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
