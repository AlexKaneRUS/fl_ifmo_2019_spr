module Expression where

import           Combinators
import           Control.Applicative (many, some, (<|>))
import           Control.Monad       (when)
import           Data.Bifunctor      (first)
import           Data.Char           (isDigit)
import           Text.Printf

import           Debug.Trace         (trace)

data Operator = Pow
              | Mul
              | Div
              | Sum
              | Minus
              | Eq
              | Neq
              | Le
              | Lt
              | Ge
              | Gt
              | Conj
              | Disj

-- Simplest abstract syntax tree for expressions: only binops are allowed
data EAst a = BinOp Operator (EAst a) (EAst a)
            | Primary a

-- Change the signature if necessary
parseExpression :: String -> Either String (EAst Integer)
parseExpression = first show . parse (parserE <* eof)

-- E $\to$ A||E | A
parserE :: ParserS (EAst Integer)
parserE = do
    a <- parserA
    (BinOp Disj a <$ betweenSpaces (string "||") <*> parserE) <|> pure a

-- A $\to$ B\&\&A | B
parserA :: ParserS (EAst Integer)
parserA = do
    b <- parserB
    (BinOp Conj b <$ betweenSpaces (string "&&") <*> parserA) <|> pure b

betweenSpaces :: ParserS a -> ParserS a
betweenSpaces = between (many space) (many space)

betweenBrackets :: ParserS a -> ParserS a
betweenBrackets p = do
    _        <- many space
    bracketM <- peek

    case bracketM of
      Just '(' -> char '(' *> betweenBrackets p <* many space <* char ')'
      _        -> p

betweenBrackets1 :: ParserS a -> ParserS a
betweenBrackets1 p = betweenSpaces (char '(') *> p <* betweenSpaces (char ')')

-- B $\to$ CNC | (E)NC | CN(E) | (E)N(E) | C
parserB :: ParserS (EAst Integer)
parserB =  flip BinOp <$> parserC <*> betweenSpaces parserN <*> (parserC <|> betweenBrackets1 parserE)
       <|> flip BinOp <$> betweenBrackets1 parserE <*> betweenSpaces parserN <*> (parserC <|> betweenBrackets1 parserE)
       <|> parserC

-- N $\to$ == | /= | $\le$ | < | $\ge$ | >
parserN :: ParserS Operator
parserN =  string "==" *> pure Eq
       <|> string "/=" *> pure Neq
       <|> string "<=" *> pure Le
       <|> string "<"  *> pure Lt
       <|> string ">=" *> pure Ge
       <|> string ">"  *> pure Gt

-- C $\to$ CPD | D
parserC :: ParserS (EAst Integer)
parserC = foldl (flip ($)) <$> parserD <*> many (fmap flip (BinOp <$> betweenSpaces parserP) <*> parserD)

-- P $\to$ + | -
parserP :: ParserS Operator
parserP =  string "+" *> pure Sum
       <|> string "-" *> pure Minus

-- D $\to$ DMF | F
parserD :: ParserS (EAst Integer)
parserD = foldl (flip ($)) <$> parserF <*> many (fmap flip (BinOp <$> betweenSpaces parserM) <*> parserF)

-- M $\to$ * | /
parserM :: ParserS Operator
parserM =  string "*" *> pure Mul
       <|> string "/" *> pure Div

-- F $\to$ $T^{\land}F$ | $(E)^{\land}F$ | (E) | T
parserF :: ParserS (EAst Integer)
parserF =  do
             tk <- parserT
             flip BinOp tk <$> parserOp <*> parserF <|> pure tk
       <|> do
             ex <- betweenBrackets1 (betweenBrackets parserE)
             flip BinOp ex <$> parserOp <*> parserF <|> pure ex
  where
    parserOp :: ParserS Operator
    parserOp = betweenSpaces $ string "^" *> pure Pow

parserT :: ParserS (EAst Integer)
parserT = fmap (Primary . fromIntegral) int

instance Show Operator where
  show Pow   = "^"
  show Mul   = "*"
  show Div   = "/"
  show Sum   = "+"
  show Minus = "-"
  show Eq    = "=="
  show Neq   = "/="
  show Le    = "<="
  show Lt    = "<"
  show Ge    = ">="
  show Gt    = ">"
  show Conj  = "&&"
  show Disj  = "||"

instance Show a => Show (EAst a) where
  show = show' 0
    where
      show' n t =
        (if n > 0 then printf "%s|_%s" (concat (replicate (n - 1) "| ")) else id)
        (case t of
                  BinOp op l r -> printf "%s\n%s\n%s" (show op) (show' (ident n) l) (show' (ident n) r)
                  Primary x -> show x)
      ident = (+1)

{-
show (BinOp Conj (BinOp Pow (Primary 1) (BinOp Sum (Primary 2) (Primary 3))) (Primary 4))

&&
|_^
| |_1
| |_+
| | |_2
| | |_3
|_4
-}
