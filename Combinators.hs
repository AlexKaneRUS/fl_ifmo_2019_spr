{-# LANGUAGE ScopedTypeVariables #-}

module Combinators where

import           Control.Applicative (Alternative (..))
import           Control.Monad       (join)
import           Data.Char           (isAlpha, isDigit, isSpace, isSymbol)
import           Data.List           (elemIndex, lookup)
import           Data.List           (union)
import           Data.Maybe          (listToMaybe)
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

newtype Parser e s ok = Parser { runParser :: Tokens s -> Either (BundleOfErrors e) (Tokens s, ok) }

type ParserS ok = Parser String Char ok

parse :: Parser e Char ok -> String -> Either (BundleOfErrors e) ok
parse p stream = snd <$> runParser p finalTokens
  where
    manyLines = lines stream
    tokens    = concat $ zipWith markLine [0..] manyLines

    finalTokens = if last stream == '\n' then tokens else init tokens

    markLine :: Int -> String -> Tokens Char
    markLine i s = zipWith (\a b -> Token (i, a) b) [0..] s ++ [Token (i, length s) '\n']

type Tokens s = [Token s]

coordsFromTokens :: Tokens s -> Maybe (Int, Int)
coordsFromTokens = fmap coords . listToMaybe

data Token s = Token { coords :: (Int, Int)
                     , symbol :: s
                     }
  deriving (Eq, Show)

type BundleOfErrors e = [ParserError e]

data ParserError e = ParserError { place :: Maybe (Int, Int)
                                 , error :: String
                                 }
  deriving (Eq, Show)

instance Functor (Parser e str) where
  fmap f p = Parser $ \s ->
    case runParser p s of
      Right (s', a) -> Right (s', f a)
      Left e        -> Left e

instance Applicative (Parser e str) where
  pure ok = Parser $ \str -> Right (str, ok)

  p <*> q = Parser $ join . ((\(s', f) -> runParser (fmap f q) s') <$>) . runParser p

instance Alternative (Parser e str) where
  p <|> q = Parser $ \s ->
    case runParser p s of
      Left e ->
        case runParser q s of
          Left e' -> Left $ e `union` e'
          x       -> x
      x      -> x

  empty = Parser $ const $ Left []

  many p = Parser $ Right . helper
    where
      helper str =
        case runParser p str of
          Right (str', a) -> (a :) <$> helper str'
          _               -> (str, [])

  some p = Parser $ \str ->
    case runParser (many p) str of
      Right (str, []) -> Left $ [ParserError (coordsFromTokens str) "Can't parse next symbol."]
      r@Right{}       -> r
      Left e          -> Left e

instance Monad (Parser e str) where
  p >>= q = Parser $ \s ->
    case runParser p s of
      Right (str, ok) -> runParser (q ok) str
      Left e          -> Left e

  fail e = Parser $ const $ Left [ParserError Nothing e]

-- Parser which always succeedes consuming no input
success :: ok -> Parser str e ok
success = pure

-- Default sequence combinator
-- If the first parser succeedes then the second parser is used
-- If the first does not succeed then the seconsd one is never tried
-- The result is collected into a pair
seq :: Parser e str a -> Parser e str b -> Parser e str (a, b)
p `seq` q = Parser $ \s ->
  case runParser p s of
    Right (str, ok) -> runParser (fmap ((,) ok) q) str
    Left e          -> Left e

checkToken :: (s -> Bool) -> Token s -> Bool
checkToken p = p . symbol

-- Parses keywords
keywords :: [String] -> Parser String Char String
keywords kws = Parser $ \str ->
  let parsed    = takeWhile (checkToken (not . isSpace)) str
      parsedStr = fmap symbol parsed
  in
  if find bor parsedStr
    then Right (dropWhile (checkToken (not . isSpace)) str, parsedStr)
    else Left [ParserError (coordsFromTokens parsed) "Can't parse keyword."]
  where
    bor = foldl insert (Trie False []) kws

-- Checks if the first element of the input is the given token
token :: Eq token => token -> Parser e token token
token = satisfy . (==)

satisfy :: (token -> Bool) -> Parser e token token
satisfy p = Parser $ \s ->
  case s of
    (t' : s') | p (symbol t') -> Right (s', symbol t')
    _                         -> Left $ [ParserError (coordsFromTokens s) "Token doesn't satisfy condition."]

-- Checks if the first character of the string is the one given
char :: Char -> Parser e Char Char
char = token

space :: Parser e Char Char
space = satisfy isSpace

letter :: Parser e Char Char
letter = satisfy isAlpha

anyChar :: Parser e Char Char
anyChar = satisfy (const True)

digit :: Parser e Char Char
digit = satisfy isDigit

int :: Parser e Char Int
int = read <$> some digit

string :: String -> Parser e Char String
string []       = pure []
string (x : xs) = (:) <$> satisfy (== x) <*> string xs

spaces1 :: Parser e Char String
spaces1 = some space

between :: Parser e token [token] -> Parser e token [token] -> Parser e token a -> Parser e token a
between l r p = l *> p <* r

eof :: Parser e Char ()
eof = Parser $ \str ->
  case str of
    []  -> pure ([], ())
    [x] | isSpace (symbol x) -> pure ([], ())
    _   -> Left $ [ParserError (coordsFromTokens str) "Can't parse EOF."]

getInput :: Parser e a (Tokens a)
getInput = Parser $ \str -> pure (str, str)

peek :: Parser e a (Maybe a)
peek = Parser $ \str ->pure (str, symbol <$> listToMaybe str)

setInput :: Tokens a -> Parser e a ()
setInput str = Parser $ \_ -> pure (str, ())

takeWhileP :: (a -> Bool) -> Parser e a [a]
takeWhileP predicate = do
    s <- getInput

    case s of
      []       -> pure []
      (x : xs) -> case predicate (symbol x) of
        True -> setInput xs >> (:) <$> pure (symbol x) <*> takeWhileP predicate
        _    -> pure []


parseList :: Parser e Char el -> Parser e Char del -> Parser e Char lbr -> Parser e Char rbr -> Int -> Parser e Char [el]
parseList elementP delimP lbrP rbrP minimumNumberElems | minimumNumberElems < 0 = fail "Invalid length."
                                                       | otherwise = (lbrP *> many space *> rbrP *> pure []) <|> do
    _  <- lbrP

    resHead <- many space *> elementP <* many space
    resTail <- many $ delimP *> many space *> elementP <* many space

    _ <- rbrP

    if length (resHead : resTail) >= minimumNumberElems
      then pure (resHead : resTail)
      else fail "Invalid length."

data Assoc = LAssoc -- left associativity
           | RAssoc -- right associativity
           | NAssoc -- not associative

-- General parser combinator for expressions
-- Binary operators are listed in the order of precedence (from lower to higher)
-- Binary operators on the same level of precedence have the same associativity
-- Binary operator is specified with a parser for the operator itself and a semantic function to apply to the operands
expression :: [(Assoc, [(Parser str b err, a -> a -> a)])] ->
              Parser str a err ->
              Parser str a err
expression ops primary = undefined

runParserUntilEof :: Foldable t => Parser (t str) ok String -> (t str) -> Either String ok
runParserUntilEof p inp =
  either (Left . id) (\(rest, ok) -> if null rest then Right ok else Left "Expected eof") (runParser p inp)
