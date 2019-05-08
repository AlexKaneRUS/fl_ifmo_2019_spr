module Expression where

import           Combinators
import           Control.Applicative (many, some, (<|>))
import           Control.Monad       (when)
import           Data.Bifunctor      (bimap, first)
import           Data.Char           (isAlphaNum, isDigit, isLower)
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
              | Neg
              | LogNeg
  deriving (Eq)

-- Simplest abstract syntax tree for expressions: only binops are allowed
data EAst a = BinOp Operator (EAst a) (EAst a)
            | UnOp Operator (EAst a)
            | Primary a
            | Var String

fromPrimary :: EAst a -> a
fromPrimary (Primary a) = a
fromPrimary _           = Prelude.error "Not primary."

-- Change the signature if necessary
-- Constructs AST for the input expression
parseExpression :: String -> Either String (EAst Integer)
parseExpression = bimap show id . parse (expression exprOpsListAST (primaryP <|> varP) (betweenBrackets1 . betweenBrackets))

parseExpressionOptimized :: String -> Either String (EAst Integer)
parseExpressionOptimized = fmap optimize . parseExpression

exprOpsListAST :: OpsList String Char String (EAst Integer)
exprOpsListAST = [ binToOps (RAssoc, [ (betweenSpaces $ string "||", BinOp Disj)
                                     ]
                            )
                 , binToOps (RAssoc, [ (betweenSpaces $ string "&&", BinOp Conj)
                                     ]
                            )
                 , binToOps (NAssoc, [ (betweenSpaces $ string "==", BinOp Eq)
                                     , (betweenSpaces $ string "/=", BinOp Neq)
                                     , (betweenSpaces $ string "<=", BinOp Le)
                                     , (betweenSpaces $ string  "<", BinOp Lt)
                                     , (betweenSpaces $ string ">=", BinOp Ge)
                                     , (betweenSpaces $ string  ">", BinOp Gt)
                                     ]
                             )
                 , binToOps (LAssoc, [ (betweenSpaces $ string "+", BinOp Sum)
                                     , (betweenSpaces $ string "-", BinOp Minus)
                                     ]
                             )
                 , binToOps (LAssoc, [ (betweenSpaces $ string "*", BinOp Mul)
                                     , (betweenSpaces $ string "/", BinOp Div)
                                     ]
                             )
                 , binToOps (RAssoc, [ (betweenSpaces $ string "^", BinOp Pow)
                                     ]
                             )
                 , unoToOps [ (betweenSpaces $ string "!", UnOp LogNeg)
                            , (betweenSpaces $ string "-", UnOp Neg)
                            ]
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

primaryP :: ParserS (EAst Integer)
primaryP = fmap (Primary . fromIntegral) int

varP :: ParserS (EAst Integer)
varP = fmap Var . (:) <$> (char '_' <|> satisfy isLower) <*> many (satisfy isAlphaNum)

instance Show Operator where
  show Pow    = "^"
  show Mul    = "*"
  show Div    = "/"
  show Sum    = "+"
  show Minus  = "-"
  show Eq     = "=="
  show Neq    = "/="
  show Le     = "<="
  show Lt     = "<"
  show Ge     = ">="
  show Gt     = ">"
  show Conj   = "&&"
  show Disj   = "||"
  show Neg    = "-"
  show LogNeg = "!"

instance Show a => Show (EAst a) where
  show = show' 0
    where
      show' n t =
        (if n > 0 then printf "%s|_%s" (concat (replicate (n - 1) "| ")) else id)
        (case t of
                  BinOp op l r -> printf "%s\n%s\n%s" (show op) (show' (ident n) l) (show' (ident n) r)
                  UnOp  op l   -> printf "%s\n%s" (show op) (show' (ident n) l)
                  Var   x      -> x
                  Primary x    -> show x)
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
executeExpression :: String -> Either String Integer
executeExpression input =
  runParserUntilEof (expression exprOpsListCalc (fromPrimary <$> primaryP) (betweenBrackets1 . betweenBrackets)) input

exprOpsListCalc :: OpsList String Char String Integer
exprOpsListCalc = [ binToOps (RAssoc, [ (betweenSpaces $ string "||", (\x y -> fromEnum' $ x /= 0 || y /= 0))
                             ]
                   )
                 , binToOps (RAssoc, [ (betweenSpaces $ string "&&", (\x y -> fromEnum' $ x /= 0 && y /= 0))
                                     ]
                            )
                 , binToOps (NAssoc, [ (betweenSpaces $ string "==", (fromEnum' <$>) <$> (==))
                            , (betweenSpaces $ string "/=", (fromEnum' <$>) <$> (/=))
                            , (betweenSpaces $ string "<=", (fromEnum' <$>) <$> (<=))
                            , (betweenSpaces $ string  "<", (fromEnum' <$>) <$> (<))
                            , (betweenSpaces $ string ">=", (fromEnum' <$>) <$> (>=))
                            , (betweenSpaces $ string  ">", (fromEnum' <$>) <$> (>))
                            ]
                   )
                 , binToOps (LAssoc, [ (betweenSpaces $ string "+", (+))
                            , (betweenSpaces $ string "-", (-))
                            ]
                   )
                 , binToOps (LAssoc, [ (betweenSpaces $ string "*", (*))
                            , (betweenSpaces $ string "/", (\x y -> round $ fromIntegral x / fromIntegral y))
                            ]
                   )
                 , binToOps (RAssoc, [ (betweenSpaces $ string "^", (\x y -> if y <= 0 then round $ fromIntegral x / fromIntegral (abs y) else x ^ y) )
                            ]
                   )
               , unoToOps [ (betweenSpaces $ string "!", (-) 1)
                          , (betweenSpaces $ string "-", (-) 0)
                          ]
                 ]

fromEnum' :: Bool -> Integer
fromEnum' b | b         = 1
            | otherwise = 0

--------------------------------------------------------------------------------
-- Optimizations.
--------------------------------------------------------------------------------

optimize :: EAst Integer -> EAst Integer
optimize (Primary a) = Primary a
optimize (Var a)     = Var a
optimize (UnOp op inner) | op == Neg, Primary x <- optimized = Primary (-x)
                         | otherwise                         = UnOp op optimized
 where
   optimized = optimize inner
optimize (BinOp op l r) | op == Mul,  Primary 0 <- l'  = Primary 0      -- 0 * a = 0
                        | op == Mul,  Primary 0 <- r'  = Primary 0      -- a * 0 = 0
                        | op == Mul,  Primary 1 <- l'  = r'             -- 1 * a = a
                        | op == Mul,  Primary 1 <- r'  = l'             -- a * 1 = a
                        | op == Sum,   Primary 0 <- l' = r'             -- 0 + a = a
                        | op == Sum,   Primary 0 <- r' = l'             -- a + 0 = 0
                        | op == Minus, Primary 0 <- r' = l'             -- a - 0 = 0
                        | op == Pow, Primary 1 <- l'   = Primary 1      -- 1 ^ a = 1
                        | op == Pow, Primary 0 <- r'   = Primary 1      -- a ^ 0 = 1
                        | op == Pow, Primary 1 <- r'   = l'             -- a ^ 1 = a
                        | op == Div, Primary 0 <- l'   = Primary 0      -- 0 / a = 0
                        | op == Div, Primary 1 <- r'   = l'             -- a / 1 = a
                        | otherwise                    = BinOp op l' r'
  where
    l' = optimize l
    r' = optimize r

