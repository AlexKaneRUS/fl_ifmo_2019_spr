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
parseExpression = first show . parse (expression exprOpsListAST primaryP (betweenBrackets1 . betweenBrackets))

exprOpsListAST :: OpsList String Char String (EAst Int)
exprOpsListAST = [ (RAssoc, [ (betweenSpaces $ string "||", BinOp Disj)
                            ]
                   )
                 , (RAssoc, [ (betweenSpaces $ string "&&", BinOp Conj)
                            ]
                   )
                 , (NAssoc, [ (betweenSpaces $ string "==", BinOp Eq)
                            , (betweenSpaces $ string "/=", BinOp Neq)
                            , (betweenSpaces $ string "<=", BinOp Le)
                            , (betweenSpaces $ string  "<", BinOp Lt)
                            , (betweenSpaces $ string ">=", BinOp Ge)
                            , (betweenSpaces $ string  ">", BinOp Gt)
                            ]
                   )
                 , (LAssoc, [ (betweenSpaces $ string "+", BinOp Sum)
                            , (betweenSpaces $ string "-", BinOp Minus)
                            ]
                   )
                 , (LAssoc, [ (betweenSpaces $ string "*", BinOp Mul)
                            , (betweenSpaces $ string "/", BinOp Div)
                            ]
                   )
                 , (RAssoc, [ (betweenSpaces $ string "^", BinOp Pow)
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
primaryP = fmap (Primary . fromIntegral) int

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
  runParserUntilEof (expression exprOpsListCalc (fromPrimary <$> primaryP) (betweenBrackets1 . betweenBrackets)) input

exprOpsListCalc :: OpsList String Char String Int
exprOpsListCalc = [ (RAssoc, [ (betweenSpaces $ string "||", (\x y -> fromEnum $ x /= 0 || y /= 0))
                             ]
                    )
                  , (RAssoc, [ (betweenSpaces $ string "&&", (\x y -> fromEnum $ x /= 0 && y /= 0))
                             ]
                    )
                  , (NAssoc, [ (betweenSpaces $ string "==", (fromEnum <$>) <$> (==))
                             , (betweenSpaces $ string "/=", (fromEnum <$>) <$> (/=))
                             , (betweenSpaces $ string "<=", (fromEnum <$>) <$> (<=))
                             , (betweenSpaces $ string  "<", (fromEnum <$>) <$> (<))
                             , (betweenSpaces $ string ">=", (fromEnum <$>) <$> (>=))
                             , (betweenSpaces $ string  ">", (fromEnum <$>) <$> (>))
                             ]
                    )
                  , (LAssoc, [ (betweenSpaces $ string "+", (+))
                             , (betweenSpaces $ string "-", (-))
                             ]
                    )
                  , (LAssoc, [ (betweenSpaces $ string "*", (*))
                             , (betweenSpaces $ string "/", (\x y -> round $ fromIntegral x / fromIntegral y))
                             ]
                    )
                  , (RAssoc, [ (betweenSpaces $ string "^", (^))
                             ]
                    )
                  ]
