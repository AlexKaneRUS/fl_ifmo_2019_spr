module Expression where

import           Combinators
import           Control.Applicative (many, some, (<|>))
import           Control.Monad       (when)
import           Data.Bifunctor      (first)
import           Data.Char           (isDigit)
import           Text.Printf

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
parseExpression = first show . parse parserE

-- E $\to$ A||E | A
parserE :: ParserS (EAst Integer)
parserE =  BinOp Disj <$> parserA <* spaces1 <* string "||" <* spaces1 <*> parserE
       <|> parserA

-- A $\to$ B\&\&A | B
parserA :: ParserS (EAst Integer)
parserA =  BinOp Conj <$> parserB <* spaces1 <* string "&&" <* spaces1 <*> parserA
       <|> parserB

betweenSpaces :: ParserS a -> ParserS a
betweenSpaces = between (many space) (many space)

betweenBrackets :: ParserS a -> ParserS a
betweenBrackets = between (pure <$> char '(' <* many space) (pure <$> char ')' <* many space)

-- B $\to$ CNC | (E)NC | CN(E) | (E)N(E) | C
parserB :: ParserS (EAst Integer)
parserB =  flip BinOp <$> parserC <*> betweenSpaces parserN <*> parserC
       <|> flip BinOp <$> betweenBrackets parserE <*> betweenSpaces parserN <*> parserC
       <|> flip BinOp <$> parserC <*> betweenSpaces parserN <*> betweenBrackets parserE
       <|> flip BinOp <$> betweenBrackets parserE <*> betweenSpaces parserN <*> betweenBrackets parserE
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
parserF =  flip BinOp <$> parserT <*> parserOp <*> parserF
       <|> flip BinOp <$> betweenBrackets parserE <*> parserOp <*> parserF
       <|> betweenBrackets parserE
       <|> parserT
  where
    parserOp :: ParserS Operator
    parserOp = betweenSpaces $ string "^" *> pure Pow

parserT :: ParserS (EAst Integer)
parserT = do
    firstChar <- satisfy isDigit

    case firstChar of
      '0' -> some (char '0') >> pure (Primary 0)
      x   -> Primary . read <$> ((x :) <$> many (satisfy isDigit))

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
