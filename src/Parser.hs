module Parser
    ( parse
    , expr
    , abstraction
    , variable
    , stmt
    , type'
    , ParseError
    ) where

import           Control.Monad
import           Text.Parsec   hiding (parse)
import qualified Text.Parsec   as P

import           Syntax

type Parser = Parsec String ()

contents :: Parser a -> Parser a
contents p = spaces *> p <* eof

parse :: Parser a -> String -> Either ParseError a
parse p = P.parse (contents p) "untyped"

parens :: Parser a -> Parser a
parens p = reserved "(" *> p <* reserved ")"

reserved :: String -> Parser String
reserved s = string s <* spaces

reserved_ :: String -> Parser ()
reserved_ = void . reserved

identifier :: Parser String
identifier = ident <* spaces
    where
        iStart  = iLetter
        iLetter = letter <|> digit <|> oneOf "_'+-*/%!&|#@"
        ident   = (:) <$> iStart <*> many iLetter

term :: Parser Expr
term = parens expr <|> literal <|> variable <|> abstraction

natural :: Parser Integer
natural = read <$> many1 digit

boolean :: Parser Bool
boolean = (true <|> false) <* spaces where
    true  = True   <$ reserved "True"
    false = False  <$ reserved "False"

number :: Parser Int
number = (fromIntegral <$> natural) <* spaces

literal :: Parser Expr
literal = Lit <$> ((LBool <$> boolean) <|> (LInt <$> number))

expr :: Parser Expr
expr = foldl1 App <$> many1 term

variable :: Parser Expr
variable = Var <$> identifier

tlit :: Parser Type
tlit = (TBool <$ reserved "Bool") <|> (TInt <$ reserved "Int")

tatom :: Parser Type
tatom = tlit <|> parens type'

type' :: Parser Type
type' = foldr1 TArrow <$> sepBy1 tatom (reserved "->")

abstraction :: Parser Expr
abstraction = do
    reserved_ "\\"
    -- args :: (String, Type)
    args <- sepBy1 arg (char ',' <* spaces)
    reserved_ "."
    body <- expr
    return $ foldr ff body args
    where
        arg :: Parser (Name, Type)
        arg = (,) <$> identifier <*> (reserved ":" *> type') <* spaces

        ff (i, t) body = Lam i t body

binding :: Parser Stmt
binding = do
    reserved_ "let"
    i <- identifier
    reserved_ "="
    e <- expr
    return $ Let i e

stmt :: Parser Stmt
stmt = try binding <|> (Expr <$> expr)
