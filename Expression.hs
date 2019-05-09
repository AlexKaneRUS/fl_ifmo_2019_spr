module Expression where

import           Combinators         hiding (error)
import           Control.Applicative (many, some, (<|>))
import           Control.Monad       (when)
import           Data.Bifunctor      (bimap, first)
import           Data.Char           (isAlphaNum, isDigit, isLower, isUpper)
import           Data.Function       (on)
import           Data.List           (groupBy, sortOn)
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
  deriving (Eq, Show)

exprOpsListAST :: OpsList String Char String Expression
exprOpsListAST = [ binToOps (RAssoc, [ (betweenSpaces $ string "||", toExpr $ BinOp Disj)
                                     , (betweenSpaces $ string "&&", toExpr $ BinOp Conj)
                                     ]
                             )
                 , binToOps (NAssoc, [ (betweenSpaces $ string "==", toExpr $ BinOp Eq)
                                     , (betweenSpaces $ string "/=", toExpr $ BinOp Neq)
                                     , (betweenSpaces $ string "<=", toExpr $ BinOp Le)
                                     , (betweenSpaces $ string  "<", toExpr $ BinOp Lt)
                                     , (betweenSpaces $ string ">=", toExpr $ BinOp Ge)
                                     , (betweenSpaces $ string  ">", toExpr $ BinOp Gt)
                                     ]
                             )
                 , binToOps (LAssoc, [ (betweenSpaces $ string "+", toExpr $ BinOp Sum)
                                     , (betweenSpaces $ string "-", toExpr $ BinOp Minus)
                                     ]
                             )
                 , binToOps (LAssoc, [ (betweenSpaces $ string "*", toExpr $ BinOp Mul)
                                     , (betweenSpaces $ string "/", toExpr $ BinOp Div)
                                     ]
                             )
                 , unoToOps [ (betweenSpaces $ string "!", toExprU $ UnOp LogNeg)
                            , (betweenSpaces $ string "-", toExprU $ UnOp Neg)
                            ]
                 , binToOps (RAssoc, [ (betweenSpaces $ string "^", toExpr $ BinOp Pow)
                                     ]
                             )
                 ]

toExpr :: (Expression -> Expression -> ArExpression) -> Expression -> Expression -> Expression
toExpr f a = ArEx . f a

toExprU :: (Expression -> ArExpression) -> Expression -> Expression
toExprU f = ArEx . f

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
betweenBrackets1 p = char '(' *> many space *> betweenBrackets p <* many space <* char ')'

expressionP :: ParserS Expression
expressionP = (iTEP <|> letVarP <|> letDataP <|> arExpressionP <|> primaryP)

iTEP :: ParserS Expression
iTEP =  ITE
    <$> (string "if" *> some space *> expressionP)
    <*> (some space *> string "then" *> some space *> expressionP)
    <*> (some space *> string "else" *> some space *> expressionP)

letVarP :: ParserS Expression
letVarP =  LetVar
       <$> (string "let" *> some space *> pVarP)
       <*> (some space *> char '=' *> some space *> expressionP)
       <*> (some space *> string "in" *> some space *> expressionP)

letDataP :: ParserS Expression
letDataP =  uncurry LetData
        <$> (string "let" *> some space *> patternMatchDataP)
        <*> (betweenSpaces1 (char '=') *> expressionP)
        <*> (betweenSpaces1 (string "in") *> expressionP)

patternMatchDataP :: ParserS (DataConstructor, [Primary])
patternMatchDataP =  (,)
                 <$> dataConstructorP
                 <*> (some space *> some (many space *> pVarP))

arExpressionP :: ParserS Expression
arExpressionP = expression exprOpsListAST primaryP betweenBrackets1

primaryP :: ParserS Expression
primaryP = Primary <$> primaryP'
  where
    primaryP' :: ParserS Primary
    primaryP' = pIntP <|> pBoolP <|> pUndirectedP <|> pDirectedP <|> pDataP <|> pFuncCallP <|> pVarP

pIntP :: ParserS Primary
pIntP = fmap PInt int

pVarP :: ParserS Primary
pVarP = fmap PVar varNameP

pBoolP :: ParserS Primary
pBoolP = PBool <$> ((True <$ string "True") <|> (False <$ string "False"))

pUndirectedP :: ParserS Primary
pUndirectedP = char '<' *> (dirToUn <$> pDirectedP) <* char '>'
  where
    dirToUn :: Primary -> Primary
    dirToUn (PDirected a b) = PUndirected a b
    dirToUn _               = error "Unexpected usage of dirToUn."

pDirectedP :: ParserS Primary
pDirectedP = do
    _ <- char '<'
    _ <- many space

    listOfVertices <- parseList int commaP lBracketP rBracketP 0

    _ <- char ','
    _ <- many space

    listOfEdges <- parseList tripleP commaP lBracketP rBracketP 0

    _ <- many space
    _ <- char '>'

    pure $ PDirected listOfVertices listOfEdges
  where
    commaP :: ParserS Char
    commaP = betweenSpaces $ char ','

    lBracketP :: ParserS Char
    lBracketP = betweenSpaces $ char '['

    rBracketP :: ParserS Char
    rBracketP = betweenSpaces $ char ']'

    tripleP :: ParserS (Int, Int, Int)
    tripleP = char '(' *> pure (,,)
           <* many space <*> int <* many space <* char ','
           <* many space <*> int <* many space <* char ','
           <* many space <*> int <* many space <* char ')'

varNameP :: ParserS VarName
varNameP = do
    name <- (:) <$> (char '_' <|> satisfy isLower) <*> many (char '_' <|> satisfy isAlphaNum)
    when (name `elem` keyWords) (fail "Variable name can't match any of keywords.")
    pure name

keyWords :: [String]
keyWords = [ "in", "if", "then", "else", "let" ]

dataConstructorP :: ParserS DataConstructor
dataConstructorP = (:) <$> satisfy isUpper <*> many letter

pDataP :: ParserS Primary
pDataP = PData <$> dataConstructorP <*> many (many space *> expressionP)

funcNameP :: ParserS FuncName
funcNameP = varNameP

pFuncCallP :: ParserS Primary
pFuncCallP = PFuncCall <$> funcNameP <*> betweenBrackets1 (many (many space *> expressionP))

dataTypeP :: ParserS DataType
dataTypeP = dataConstructorP

typeP :: ParserS Type
typeP = arrowP <|> intP <|> boolP <|> directedP <|> undirectedP <|> dataTypeP'

intP :: ParserS Type
intP = Int <$ string "Int"

boolP :: ParserS Type
boolP = Bool <$ string "Bool"

directedP :: ParserS Type
directedP = Directed <$ string "Directed"

undirectedP :: ParserS Type
undirectedP = Undirected <$ string "Undirected"

dataTypeP' :: ParserS Type
dataTypeP' = fmap DataType dataTypeP

notArrowP :: ParserS Type
notArrowP = intP <|> boolP <|> directedP <|> undirectedP <|> dataTypeP' <|> betweenBrackets1 typeP

arrowP :: ParserS Type
arrowP = foldr1 Arrow <$> ((:) <$> notArrowP <*> some (betweenSpaces (string "->") *> notArrowP))

aDTP :: ParserS (DataType, [Constructor])
aDTP =  (,)
    <$> (string "data" *> betweenSpaces dataTypeP <* betweenSpaces (char '='))
    <*> ((++) <$> many (constructorP <* betweenSpaces (char '|')) <*> (fmap pure constructorP))
  where
    constructorP :: ParserS Constructor
    constructorP = Constructor <$> dataConstructorP <*> many (some space *> (notArrowP <|> betweenBrackets1 arrowP))

functionP :: ParserS (FuncName, Func)
functionP =  (,)
         <$> funcNameP
         <*> funcP
  where
    funcP :: ParserS Func
    funcP =  Func
         <$> betweenBrackets1 (((:) <$> funcArgP <*> many (char ',' *> betweenSpaces funcArgP)) <|> pure [])
         <*> (betweenSpaces (char ':') *> typeP)
         <*> (betweenSpaces (char '=') *> between (betweenSpaces (string "{")) (betweenSpaces (string "}")) expressionP)

    funcArgP :: ParserS FuncArg
    funcArgP = (uncurry PatternArg <$> patternMatchDataP) <|> (VarArg <$> pVarP)

gradskellP :: ParserS GradskellAst
gradskellP =  GradskellProgram
          <$> (M.fromList <$> many (many space *> aDTP))
          <*> (M.fromList . ungroups <$> many (many space *> functionP))
          <*> (many space *> expressionP)

ungroups :: (Eq a, Ord a) => [(a, b)] -> [(a, [b])]
ungroups = fmap (\x -> (fst $ head x, fmap snd x)) . groupBy ((==) `on` fst) . sortOn fst


--------------------------------------------------------------------------------
-- Gradskell.
--------------------------------------------------------------------------------

type VarName = String

type DataConstructor = String

data GradskellAst = GradskellProgram (Map DataType [Constructor]) (Map FuncName [Func]) Expression
  deriving (Eq, Show)

type DataType = String
data Constructor = Constructor DataConstructor [Type]
  deriving (Eq, Show)
data Type = Int | Bool | Directed | Undirected | DataType DataType | Arrow Type Type
  deriving (Eq, Show)

type FuncName = String
data Func = Func [FuncArg] Type Body
  deriving (Eq, Show)
data FuncArg = VarArg Primary | PatternArg DataConstructor [Primary]
  deriving (Eq, Show)
type Body = Expression

data Expression = ITE Expression Expression Expression
                | LetVar Primary Expression Expression
                | LetData DataConstructor [Primary] Expression Expression
                | ArEx ArExpression
                | Primary Primary
  deriving (Eq, Show)

data ArExpression = BinOp Operator Expression Expression
                  | UnOp Operator Expression
  deriving (Eq, Show)

data Primary = PInt Int
             | PBool Bool
             | PDirected [Int] [(Int, Int, Int)]
             | PUndirected [Int] [(Int, Int, Int)]
             | PData DataConstructor [Expression]
             | PVar VarName
             | PFuncCall FuncName [Expression]
  deriving (Eq, Show)

