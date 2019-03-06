module Tokenizer
  where

import           Combinators
import           Control.Applicative (Alternative (..))
import           Control.Monad       (when)
import           Data.Bifunctor      (first)

data Tok = Ident String
           | KeyWord String
           | Number Integer -- Change Number type if you work with something other than Int
           | EOF
  deriving (Show, Eq)

tokenize :: String -> Either String [Tok]
tokenize = first (concat . fmap show) . parse tokensP

tokensP :: ParserS [Tok]
tokensP = do
    begTok  <- many space *> (tokenP <|> eof *> pure EOF)
    resToks <- many $ some space *> (tokenP <|> eof *> pure EOF)

    many space

    curStream <- getInput
    when (not (null curStream)) (fail "Couldn't parse whole string.")

    pure $ begTok : resToks

--------------------------------------------------------------------------------
-- Python lexical structure.
--------------------------------------------------------------------------------

tokenP :: ParserS Tok
tokenP = keyWordP <|> intP <|> identP

-- Identifier in Python looks like this:
--   ident  = ('_' | letter) ident'
--   ident' = ('_' | letter | digit) ident'
--   letter = 'a'...'z','A'...'Z'
--   digit  = '0'...'9'
--
identP :: ParserS Tok
identP = Ident <$> ((:) <$> (char '_' <|> letter) <*> many (char '_' <|> letter <|> digit))

-- There is fixed number of keywords in Python. Should check them all.
--
keyWordP :: ParserS Tok
keyWordP = KeyWord <$> keywords listOfKeyWords
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
intP :: ParserS Tok
intP = do
    firstChar <- anyChar

    when (firstChar `notElem` digits) (fail "Can't parse Number.")

    case firstChar of
      '0' -> many ((char '_' *> char '0') <|> char '0') >> pure (Number 0)
      x   -> Number . read . filter (/= '_')
             <$> ((x :) <$> many ((char '_' *> satDigit) <|> satDigit))
  where
    digits = ['0'..'9']

    satDigit :: ParserS Char
    satDigit = satisfy (`elem` digits)
