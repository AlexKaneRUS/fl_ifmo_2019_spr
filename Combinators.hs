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

-- Parsing result is some payload and a suffix of the input which is yet to be parsed
-- newtype Parser str ok = Parser { runParser :: str -> Maybe (str, ok) }

newtype Parser str ok err = Parser { runParser :: str -> Either err (str, ok) }

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

-- Default sequence combinator
-- If the first parser succeedes then the second parser is used
-- If the first does not succeed then the seconsd one is never tried
-- The result is collected into a pair
seq :: Parser str a -> Parser str b -> Parser str (a, b)
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

-- Checks if the first character of the string is the one given
char :: Char -> Parser String Char
char = token

space :: Parser String Char
space = satisfy isSpace

letter :: Parser String Char
letter = satisfy isAlpha

anyChar :: Parser String Char
anyChar = satisfy (const True)

digit :: Parser String Char
digit = satisfy isDigit

int :: Parser String Int
int = read <$> some digit

eof :: Parser String ()
eof = Parser $ \str -> if null str then Just ([], ()) else Nothing

getInput :: Parser [a] [a]
getInput = Parser $ \str -> Just (str, str)

setInput :: [a] -> Parser [a] ()
setInput str = Parser $ \_ -> Just (str, ())

takeWhileP :: (a -> Bool) -> Parser [a] [a]
takeWhileP predicate = do
    s <- getInput

    case s of
      []       -> pure []
      (x : xs) -> case predicate x of
        True -> setInput xs >> (:) <$> pure x <*> takeWhileP predicate
        _    -> pure []


parseList :: Parser String el -> Parser String del -> Parser String lbr -> Parser String rbr -> Int -> Parser String [el]
parseList elementP delimP lbrP rbrP minimumNumberElems | minimumNumberElems < 0 = fail "Invalid length."
                                                       | otherwise = (lbrP *> many space *> rbrP *> pure []) <|> do
    _  <- lbrP

    resHead <- many space *> elementP <* many space
    resTail <- many $ delimP *> many space *> elementP <* many space

    _ <- rbrP

    if length (resHead : resTail) >= minimumNumberElems
      then pure (resHead : resTail)
      else fail "Invalid length."
