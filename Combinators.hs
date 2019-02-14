module Combinators where

import Prelude hiding (fail, fmap)

-- Parsing result is some payload and a suffix of the input which is yet to be parsed
newtype Parser str ok = Parser { runParser :: str -> Maybe (str, ok) }

-- Parser which always succeedes consuming no input
success :: ok -> Parser str ok
success ok = Parser $ \s -> Just (s, ok)

-- Parser which fails no mater the input
fail :: Parser str ok
fail = Parser $ const Nothing

-- Biased choice: if the first parser succeedes, the second is never run
(<|>) :: Parser str ok -> Parser str ok -> Parser str ok
p <|> q = Parser $ \s ->
  case runParser p s of
    Nothing -> runParser q s
    x -> x

-- Default sequence combinator
-- If the first parser succeedes then the second parser is used
-- If the first does not succeed then the second one is never tried
-- The result is collected into a pair
seq :: Parser str a -> Parser str b -> Parser str (a, b)
p `seq` q = undefined

-- Monadic sequence combinator
(>>=) :: Parser str a -> (a -> Parser str b) -> Parser str b
p >>= q = undefined

-- Applicative sequence combinator
(<*>) :: Parser str (a -> b) -> Parser str a -> Parser str b
p <*> q = undefined

-- Applies a function to the parsing result, if parser succeedes
fmap :: (a -> b) -> Parser str a -> Parser str b
fmap f p = Parser $ \s ->
  case runParser p s of
    Just (s', a) -> Just (s', f a)
    _ -> Nothing

-- Applies a parser once or more times
some :: Parser str a -> Parser str [a]
some p = undefined

-- Applies a parser zero or more times
many :: Parser str a -> Parser str [a]
many p = undefined

-- Parses keywords 
keywords :: [String] -> Parser String String
keywords kws = undefined

-- Checks if the first element of the input is the given token
token :: Eq token => token -> Parser [token] token
token t = Parser $ \s ->
  case s of
    (t' : s') | t == t' -> Just (s', t)
    _ -> Nothing

-- Checks if the first character of the string is the one given
char :: Char -> Parser String Char
char = token
