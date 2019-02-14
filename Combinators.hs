<<<<<<< HEAD
{-# LANGUAGE ScopedTypeVariables #-}

module Combinators where

import           Control.Applicative (Alternative (..))
import           Control.Monad       (join)
import           Data.Char           (isAlpha, isDigit, isSpace, isSymbol)
import           Data.List           (elemIndex, lookup)
import           Data.Tuple          (swap)

data Trie = Trie Bool [(Char, Trie)] deriving (Eq, Show)

find :: Trie -> String -> Bool
find (Trie True _) []    = True
find (Trie False _) []   = False
find (Trie _ l) (x : xs) = maybe False (flip find xs) $ x `lookup` l

insert :: Trie -> String -> Trie
insert t s = if find t s then t
             else helper t s
  where
    helper :: Trie -> String -> Trie
    helper (Trie _ l) []       = Trie True l
    helper (Trie b l) (x : xs) = Trie b $ maybe ((x, helper (Trie False []) xs) : l) replace $ elemIndex x $ fst <$> l
      where
        replace :: Int -> [(Char, Trie)]
        replace i = take i l ++ [(x, helper (snd $ l !! i) xs)] ++ drop (i + 1) l
=======
module Combinators where

import Prelude hiding (fail, fmap)
>>>>>>> Second homework

-- Parsing result is some payload and a suffix of the input which is yet to be parsed
newtype Parser str ok = Parser { runParser :: str -> Maybe (str, ok) }

<<<<<<< HEAD
instance Functor (Parser str) where
  fmap f p = Parser $ \s ->
    case runParser p s of
      Just (s', a) -> Just (s', f a)
      _            -> Nothing

instance Applicative (Parser str) where
  pure ok = Parser $ \str -> Just (str, ok)

  p <*> q = Parser $ join . ((\(s', f) -> runParser (fmap f q) s') <$>) . runParser p

instance Alternative (Parser str) where
  p <|> q = Parser $ \s ->
    case runParser p s of
      Nothing -> runParser q s
      x       -> x

  empty = Parser $ const Nothing

  many p = Parser $ Just . helper
    where
      helper str = case runParser p str of
        Just (str', a) -> (a :) <$> helper str'
        _              -> (str, [])

  some p = Parser $ \str -> case runParser (many p) str of
    Just (_, []) -> Nothing
    r@Just{}     -> r
    _            -> Nothing


instance Monad (Parser str) where
  p >>= q = Parser $ \s ->
    case runParser p s of
      Just (str, ok) -> runParser (q ok) str
      _              -> Nothing

  fail _ = Parser $ const Nothing

-- Parser which always succeedes consuming no input
success :: ok -> Parser str ok
success = pure
=======
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
>>>>>>> Second homework

-- Default sequence combinator
-- If the first parser succeedes then the second parser is used
-- If the first does not succeed then the second one is never tried
-- The result is collected into a pair
seq :: Parser str a -> Parser str b -> Parser str (a, b)
<<<<<<< HEAD
p `seq` q = Parser $ \s ->
  case runParser p s of
    Just (str, ok) -> runParser (fmap ((,) ok) q) str
    _              -> Nothing

-- Parses keywords
keywords :: [String] -> Parser String String
keywords kws = Parser $ \str ->
  let parsed = takeWhile (not . isSpace) str in
  if find bor parsed
    then Just (dropWhile (not . isSpace) str, parsed)
    else Nothing
  where
    bor = foldl insert (Trie False []) kws

-- Checks if the first element of the input is the given token
token :: Eq token => token -> Parser [token] token
token = satisfy . (==)

satisfy :: Eq token => (token -> Bool) -> Parser [token] token
satisfy p = Parser $ \s ->
  case s of
    (t' : s') | p t' -> Just (s', t')
    _         -> Nothing
=======
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
>>>>>>> Second homework

-- Checks if the first character of the string is the one given
char :: Char -> Parser String Char
char = token
<<<<<<< HEAD

space :: Parser String Char
space = satisfy isSpace

letter :: Parser String Char
letter = satisfy isAlpha

anyChar :: Parser String Char
anyChar = satisfy (const True)

digit :: Parser String Char
digit = satisfy isDigit

eof :: Parser String ()
eof = Parser $ \str -> if null str then Just ([], ()) else Nothing

getInput :: Parser String String
getInput = Parser $ \str -> Just (str, str)
=======
>>>>>>> Second homework
