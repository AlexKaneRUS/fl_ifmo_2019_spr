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

fromPrimary :: EAst a -> a
fromPrimary (Primary a) = a
fromPrimary _           = Prelude.error "Not primary."

-- Change the signature if necessary
-- Constructs AST for the input expression
parseExpression :: String -> Either String (EAst Int)
parseExpression = first show . parse (expression exprOpsListAST primaryP betweenBrackets)

exprOpsListAST :: OpsList String Char String (EAst Int)
exprOpsListAST = [ (RAssoc, [ (betweenSpaces1 $ string "||", BinOp Disj)
                            , (betweenSpaces1 $ string "&&", BinOp Conj)
                            ]
                   )
                 , (NAssoc, [ (betweenSpaces1 $ string "==", BinOp Eq)
                            , (betweenSpaces1 $ string "/=", BinOp Neq)
                            , (betweenSpaces1 $ string "<=", BinOp Le)
                            , (betweenSpaces1 $ string  "<", BinOp Lt)
                            , (betweenSpaces1 $ string ">=", BinOp Ge)
                            , (betweenSpaces1 $ string  ">", BinOp Gt)
                            ]
                   )
                 , (LAssoc, [ (betweenSpaces1 $ string "+", BinOp Sum)
                            , (betweenSpaces1 $ string "-", BinOp Minus)
                            ]
                   )
                 , (LAssoc, [ (betweenSpaces1 $ string "*", BinOp Mul)
                            , (betweenSpaces1 $ string "/", BinOp Div)
                            ]
                   )
                 , (RAssoc, [ (betweenSpaces1 $ string "^", BinOp Pow)
                            ]
                   )
                 ]

betweenSpaces :: ParserS a -> ParserS a
betweenSpaces = between (many space) (many space)

betweenSpaces1 :: ParserS a -> ParserS a
betweenSpaces1 = between (some space) (some space)

betweenBrackets :: ParserS a -> ParserS a
betweenBrackets p = do
    _        <- many space
    bracketM <- peek

    case bracketM of
      Just '(' -> char '(' *> betweenBrackets p <* many space <* char ')'
      _        -> p

betweenBrackets1 :: ParserS a -> ParserS a
betweenBrackets1 p = betweenSpaces (char '(') *> p <* betweenSpaces (char ')')

primaryP :: ParserS (EAst Int)
primaryP = do
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

-- Change the signature if necessary
-- Calculates the value of the input expression
executeExpression :: String -> Either String Int
executeExpression input =
  runParserUntilEof (expression exprOpsListCalc (fromPrimary <$> primaryP) (betweenBrackets . betweenBrackets1)) input

exprOpsListCalc :: OpsList String Char String Int
exprOpsListCalc = [ (RAssoc, [ (betweenSpaces1 $ string "||", (\x y -> fromEnum $ x >= 0 || y >= 0))
                             , (betweenSpaces1 $ string "&&", (\x y -> fromEnum $ x >= 0 && y >= 0))
                             ]
                   )
                 , (NAssoc, [ (betweenSpaces1 $ string "==", (fromEnum <$>) <$> (==))
                            , (betweenSpaces1 $ string "/=", (fromEnum <$>) <$> (/=))
                            , (betweenSpaces1 $ string "<=", (fromEnum <$>) <$> (<=))
                            , (betweenSpaces1 $ string  "<", (fromEnum <$>) <$> (<))
                            , (betweenSpaces1 $ string ">=", (fromEnum <$>) <$> (>=))
                            , (betweenSpaces1 $ string  ">", (fromEnum <$>) <$> (>))
                            ]
                   )
                 , (LAssoc, [ (betweenSpaces1 $ string "+", (+))
                            , (betweenSpaces1 $ string "-", (-))
                            ]
                   )
                 , (LAssoc, [ (betweenSpaces1 $ string "*", (*))
                            , (betweenSpaces1 $ string "/", (\x y -> round $ fromIntegral x / fromIntegral y))
                            ]
                   )
                 , (RAssoc, [ (betweenSpaces1 $ string "^", (^))
                            ]
                   )
                 ]
