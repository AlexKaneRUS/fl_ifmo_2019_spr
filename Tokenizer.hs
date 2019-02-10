module Tokenizer
  where

import           Control.Monad    (when)
import           Text.Parsec      (Parsec, choice, eof, getInput, many, many1,
                                   parse, try, (<|>))
import           Text.Parsec.Char (anyChar, char, digit, letter, satisfy, space,
                                   string)

import           Debug.Trace      (trace)

type Parser = Parsec String ()

data Token = Ident String
           | KeyWord String
           | Number Int     -- Change Number type if you work with something other than Int
           | EOF
  deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize = either (error . show) id . parse tokensP "Tokens parser."

tokensP :: Parser [Token]
tokensP = do
    begToken  <- many space *> (tokenP <|> eof *> pure EOF)
    resTokens <- many $ many1 space *> (tokenP <|> eof *> pure EOF)

    many space

    curStream <- getInput
    when (not (null curStream)) (fail "Couldn't parse whole string.")

    pure $ filter (/= EOF) $ begToken : resTokens

--------------------------------------------------------------------------------
-- Python lexical structure.
--------------------------------------------------------------------------------

tokenP :: Parser Token
tokenP = try keyWordP <|> try intP <|> try identP

-- Identifier in Python looks like this:
--   ident  = ('_' | letter) ident'
--   ident' = ('_' | letter | digit) ident'
--   letter = 'a'...'z','A'...'Z'
--   digit  = '0'...'9'
--
identP :: Parser Token
identP = Ident <$> ((:) <$> (char '_' <|> letter) <*> many (char '_' <|> letter <|> digit))

-- There is fixed number of keywords in Python. Should check them all.
--
keyWordP :: Parser Token
keyWordP = KeyWord <$> (choice $ fmap (try . string) listOfKeyWords)
  where
    listOfKeyWords = [ "False", "await", "else", "import", "pass"
                     , "None", "break", "except", "in", "raise"
                     , "True", "class", "finally", "is", "return"
                     , "and", "continue", "for", "lambda", "try"
                     , "as", "def", "from", "nonlocal", "while"
                     , "assert", "del", "global", "not", "with"
                     , "async", "elif", "if", "or", "yield"
                     ]

-- Decimal integer in Python looks like this:
--   decinteger   = nonzerodigit ('_'? digit)* | '0'+ ('_'? '0')*
--   nonzerodigit = '1'...'9'
--   digit        = '0'...'9'
--
intP :: Parser Token
intP = do
    firstChar <- anyChar

    when (firstChar == '_' || firstChar `notElem` digits) (fail "Can't parse Number.")

    case firstChar of
      '0' -> many (try (char '_' *> char '0') <|> char '0') >> pure (Number 0)
      x   -> Number . read . filter (/= '_')
             <$> ((x :) <$> many (try (char '_' *> satDigit) <|> satDigit))
  where
    digits = ['0'..'9']

    satDigit :: Parser Char
    satDigit = satisfy (`elem` digits)
