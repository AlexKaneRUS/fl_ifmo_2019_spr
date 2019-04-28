module Expression where

import           Combinators
import           Control.Applicative (many, some, (<|>))
import           Control.Monad       (when)
import           Data.Bifunctor      (bimap, first)
import           Data.Char           (isAlphaNum, isDigit, isLower)
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as M
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
              | Neg
              | LogNeg
  deriving (Eq)

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

-- Simplest abstract syntax tree for expressions: only binops are allowed
data GradskellAst a = BinOp Operator (EAst a) (EAst a)
            | UnOp Operator (EAst a)
            | Primary a
            | Var String

exprOpsListAST :: OpsList String Char String (GradskellAst Int)
exprOpsListAST = [ binToOps (RAssoc, [ (betweenSpaces $ string "||", BinOp Disj)
                                     , (betweenSpaces $ string "&&", BinOp Conj)
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
                 , unoToOps [ (betweenSpaces $ string "!", UnOp LogNeg)
                            , (betweenSpaces $ string "-", UnOp Neg)
                            ]
                 , binToOps (RAssoc, [ (betweenSpaces $ string "^", BinOp Pow)
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

primaryP :: ParserS (GradskellAst Int)
primaryP = do
    firstChar <- satisfy isDigit

    case firstChar of
      '0' -> many (char '0') >> pure (Primary 0)
      x   -> Primary . read <$> ((x :) <$> many (satisfy isDigit))

varP :: ParserS (GradskellAst Int)
varP = fmap Var . (:) <$> (char '_' <|> satisfy isLower) <*> many (satisfy isAlphaNum)

--------------------------------------------------------------------------------
-- Optimizations.
--------------------------------------------------------------------------------

optimize :: GradskellAst Int -> GradskellAst Int
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


--------------------------------------------------------------------------------
-- Gradskell.
--------------------------------------------------------------------------------

type VarName = String
type FuncName = String

type DataType = String
type DataConstructor = String

data GradskellAst' = GradskellProgram [DataType] (Map FuncName Func)

data Func = Func [FuncArg] Type Body

type Body = Expression

data FuncArg = VarArg VarName | PatternArg DataConstructor [VarName]

infixl 5 :->

data Type = Int | Bool | Directed | Undirected | DataType DataType | Type :-> Type

data Expression = BinOp Operator (EAst a) (EAst a)
                | UnOp Operator (EAst a)
                | PrimaryEx Primary
                | Var String

data Primary = PInt Int
             | PBool Bool
             | PDirected Directed
             | PUndirected Undirected
             | PData DataConstructor [DataArg]

data DataArg = DArgVar VarName | DArgExp Expression

-- Program.
-- Pr  $\to$ Data Pr | Pr'
-- Pr' $\to$ Fu Pr | Fu | Ex

