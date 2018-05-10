{-# LANGUAGE OverloadedStrings #-}

module Parser
       ( expression
       , operator
       , file
       ) where

import Data.Text
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Text.Megaparsec.Expr

import Arithmetic
import Operators

type Parser = Parsec Void Text

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space

symbol :: Text -> Parser Text
symbol = Lexer.symbol space

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Int
integer = lexeme (Lexer.signed space Lexer.decimal)

lit :: Parser Expr
lit = Lit <$> integer

name :: Parser Text
name = lexeme (flip (snoc . pack) <$> letterChar <*> many alphaNumChar)

variable :: Parser Expr
variable = Var <$> name

expression :: Parser Expr
expression = lexeme $ makeExprParser term operators

operators :: [[Operator Parser Expr]]
operators =
    [ [ InfixL (Mul <$ symbol "*")
      , InfixL (Div <$ symbol "/")
      ]
    , [ InfixL (Add <$ symbol "+")
      , InfixL (Sub <$ symbol "-")
      ]
    ]

term :: Parser Expr
term = parens (initlet <|> expression) <|> variable <|> lit

initlet :: Parser Expr
initlet = (\_ var _ value _ expr -> Let var value expr) <$>
    symbol "let" <*> name <*> symbol "=" <*> expression <*> symbol "in" <*> expression

operator :: Parser Oper
operator = (\_ var _ expr -> Init var expr) <$> symbol "mut" <*> name <*> symbol "=" <*> expression
    <|> (\_ var _ from _ to _ list _ -> For var from to list)
        <$> symbol "for" <*> name <*> symbol "=" <*> expression <*> symbol "to"
        <*> expression <*> symbol "{" <*> many operator <*> symbol "}"
    <|> (\var _ expr -> Set var expr) <$> name <*> symbol "=" <*> expression
    <|> (\_ expr -> Print expr) <$> symbol "<" <*> expression
    <|> (\_ var -> Read var) <$> symbol ">" <*> name

file :: Parser [Oper]
file = many operator <* eof
