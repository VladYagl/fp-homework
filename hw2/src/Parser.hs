{-# LANGUAGE LambdaCase #-}

module Parser
       ( Parser(..)
       , ok
       , eof
       , satisfy
       , element
       , stream
       , bracketSeq
       , digit
       , integer
       , intLists
       ) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Char

newtype Parser s a = Parser {runParser :: [s] -> Maybe (a, [s])}

instance Functor (Parser s) where
    fmap f (Parser parser) = Parser (fmap (first f) . parser)

instance Applicative (Parser s) where
    pure a = Parser $ \s -> Just (a, s)

    Parser pf <*> Parser pa = Parser $ pf >=> (\(f, t) -> pa t >>= (\(a, r) -> Just (f a, r)))

instance Monad (Parser s) where
    Parser pa >>= f = Parser $ pa >=> (\(a, t) -> runParser (f a) t)

instance Alternative (Parser s) where
    empty = Parser $ const Nothing

    a <|> b = Parser $ \s -> case runParser a s of
        Nothing -> runParser b s
        Just x  -> Just x

ok :: Parser s ()
ok = Parser $ \s -> Just ((), s)

eof :: Parser s ()
eof = Parser $ \case
    [] -> Just ((), [])
    _  -> Nothing

satisfy :: (s -> Bool) -> Parser s s
satisfy p = Parser $ \case
    [] -> Nothing
    (x: xs) -> if p x then Just (x, xs) else Nothing

element :: (Eq s) => s -> Parser s s
element c = satisfy (== c)

add :: Parser s a -> Parser s [a] -> Parser s [a]
add a as = (:) <$> a <*> as

stream :: (Eq s) => [s] -> Parser s [s]
stream = foldr (add . element) ([] <$ ok)

-- *************** --

bracketSeq :: Parser Char ()
bracketSeq = brackets *> eof
  where
    brackets = (brackets <* brackets) <|> element '(' <* brackets <* element ')' <|> ' ' <$ stream ""

-- *************** --

digit :: Parser Char Int
digit = digitToInt <$> satisfy isDigit
-- digit = foldr1 (<|>) (map (\c -> digitToInt c <$ element c) ['0'..'9'])

integer :: Parser Char Int
integer = sign <*> (foldl (\rest a -> rest * 10 + a) 0 <$> some digit)
    where
        sign = id <$ element '+' <|> negate <$ element '-' <|> id <$ ok


whiteSpace :: Parser Char ()
whiteSpace = many (element ' ' <|> element '\t' <|> element '\n') *> ok

comma :: Parser Char ()
comma = whiteSpace *> element ',' *> whiteSpace *> ok

intList :: Parser Char [Int]
intList = Parser $ runParser (whiteSpace *> integer) >=>
    (\(len, t) -> if len > 0
        then runParser (comma *> intListLen len) t
        else Just ([], t))
  where
    intListLen n = foldr (\a b -> (:) <$> a <*> (comma *> b)) ((: []) <$> integer) (replicate (n - 1) integer)

intLists :: Parser Char [[Int]]
intLists = (:) <$> intList <*> many (comma *> intList)


