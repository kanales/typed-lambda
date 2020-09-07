module Parser 
    ( parse
    , expr
    , abstraction
    , variable
    , stmt
    ) where

import           Control.Monad
import           Text.Parsec hiding (parse)
import qualified Text.Parsec as P
import           Text.Parsec.Char

import Syntax

type Parser = Parsec String ()

contents :: Parser a -> Parser a
contents p = spaces *> p <* eof

parse :: Parser a -> String -> Either ParseError a
parse p = P.parse (contents p) "untyped"

parens :: Parser a -> Parser a
parens p = reserved "(" *> p <* reserved ")"

reserved :: String -> Parser String
reserved s = string s <* spaces

identifier :: Parser String
identifier = ident <* spaces
    where 
        iStart  = letter
        iLetter = letter <|> digit <|> oneOf "_'"
        ident   = (:) <$> iStart <*> many iLetter

term :: Parser Expr
term = parens expr <|> variable <|> number <|> abstraction

natural :: Parser Integer
natural = read <$> many1 digit

number :: Parser Expr
number = (Lit . fromIntegral <$> natural) <* spaces

expr :: Parser Expr
expr = foldl1 App <$> many1 term

variable :: Parser Expr
variable = Var <$> identifier

abstraction :: Parser Expr
abstraction = do
    reserved "\\"
    args <- many1 identifier
    reserved "."
    body <- expr
    return $ foldr Lam body args

binding :: Parser Stmt
binding = do
    reserved "let"
    id <- identifier
    reserved "="
    ex <- expr
    return $ Let id ex

stmt :: Parser Stmt
stmt = binding <|> (Eval <$> expr)