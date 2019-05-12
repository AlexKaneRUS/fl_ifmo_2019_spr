{-# LANGUAGE RecordWildCards #-}

module Gradskell where

import           Combinators                  hiding (error)
import           Control.Applicative          (many, some, (<|>))
import           Control.Monad                (when)
import           Control.Monad.State          (StateT (..), evalStateT, get,
                                               modify)
import           Control.Monad.Trans.Class    (MonadTrans, lift)
import           Control.Monad.Trans.Identity (runIdentityT)
import           Data.Bifunctor               (bimap, first)
import           Data.Char                    (isAlphaNum, isDigit, isLower,
                                               isUpper)
import           Data.Either                  (lefts)
import           Data.Function                (on)
import           Data.List                    (groupBy, nubBy, sortOn)
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as M
import           Data.Semigroup               ((<>))
import           Text.Printf

import           Debug.Trace                  (trace)

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
expressionP = betweenBrackets (iTEP <|> letVarP <|> letDataP <|> arExpressionP <|> primaryP)

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

    when (null listOfVertices) (fail "Empty list of vertices for graph.")

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
keyWords = [ "in", "if", "then", "else", "let", "data" ]

dataConstructorP :: ParserS DataConstructor
dataConstructorP = (:) <$> satisfy isUpper <*> many (satisfy isAlphaNum)

pDataP :: ParserS Primary
pDataP = PData <$> dataConstructorP <*> many (many space *> (primaryP <|> betweenBrackets1 expressionP))

funcNameP :: ParserS FuncName
funcNameP = varNameP

pFuncCallP :: ParserS Primary
pFuncCallP =  PFuncCall
          <$> funcNameP
          <*> (betweenBrackets1 (((:) <$> expressionP <*> many (betweenSpaces (char ',') *> expressionP))
          <|> pure []))

dataTypeP :: ParserS DataType
dataTypeP = dataConstructorP

typeP :: ParserS Type
typeP = arrowP <|> intP <|> boolP <|> directedP <|> undirectedP <|> unitP <|> dataTypeP'

intP :: ParserS Type
intP = Int <$ string "Int"

boolP :: ParserS Type
boolP = Bool <$ string "Bool"

unitP :: ParserS Type
unitP = Unit <$ string "()"

directedP :: ParserS Type
directedP = Directed <$ string "Directed"

undirectedP :: ParserS Type
undirectedP = Undirected <$ string "Undirected"

dataTypeP' :: ParserS Type
dataTypeP' = fmap DataType dataTypeP

notArrowP :: ParserS Type
notArrowP = intP <|> boolP <|> directedP <|> undirectedP <|> dataTypeP' <|> unitP <|> betweenBrackets1 typeP

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

data GradskellAst = GradskellProgram { dataTypes :: Map DataType [Constructor]
                                     , functions :: Map FuncName [Func]
                                     , program   :: Expression
                                     }
  deriving (Eq, Show)

type DataType = String
data Constructor = Constructor { constructor :: DataConstructor
                               , argTypes    :: [Type]
                               }
  deriving (Eq, Show)
data Type = Int | Bool | Directed | Undirected | DataType DataType | Arrow Type Type | Unit
  deriving (Eq, Ord, Show)

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

--------------------------------------------------------------------------------
-- Type checking.
--------------------------------------------------------------------------------

inferTypeForGradskellAst :: GradskellAst -> Either [String] Type
inferTypeForGradskellAst GradskellProgram{..} = do
    constM <- getConstructorsMap dataTypes
    funcM  <- first pure $ getFunctionsMap constM functions

    bimap pure fst $ evalStateT (inferExpressionType program) (mempty, (constM, funcM))

type ConstructorsMap = Map DataConstructor (Type, [Type])
type FuncMap         = Map (FuncName, [(Type, Maybe DataConstructor)]) Type

getConstructorsMap :: Map DataType [Constructor] -> Either [String] ConstructorsMap
getConstructorsMap dataTypesM | null errorList                         = pure res
                              | nubBy ((==) `on` fst) forMap /= forMap = Left ["Duplicating constructor names."]
                              | otherwise                              = Left errorList
  where
    dataTypeNames = M.keys dataTypesM

    checkedDataTypes = M.elems $ M.mapWithKey checkConstructors dataTypesM
    errorList        = lefts checkedDataTypes

    forMap = concatMap (\(t, l) -> fmap (\x -> (constructor x, (DataType t, argTypes x))) l) $ M.toList dataTypesM
    res    = M.fromList forMap

    checkConstructors :: DataType -> [Constructor] -> Either String ()
    checkConstructors dt = eConcat . fmap checkConstructor
      where
        checkConstructor :: Constructor -> Either String ()
        checkConstructor (Constructor co types) = eConcat $ fmap checkType types
          where
            checkType :: Type -> Either String ()
            checkType (DataType dt') | dt' `elem` dataTypeNames = pure ()
                                     | otherwise                = Left dtError
              where
                dtError = "Data type " <> dt <> ", constructor " <> co <> ": No such data type " <> dt' <> "."
            checkType (Arrow l r) | Right _ <- checkType l = checkType r
                                  | otherwise              = checkType l
            checkType _           = pure ()

eConcat :: [Either String ()] -> Either String ()
eConcat = fmap (const ()) . sequence

type VarMap = Map VarName Type
type InferState = StateT (VarMap, (ConstructorsMap, FuncMap)) (Either String)

getFunctionsMap :: Map DataConstructor (Type, [Type]) -> Map FuncName [Func] -> Either String FuncMap
getFunctionsMap constM fM | nubBy ((==) `on` fst) forMap /= forMap = Left "Duplicating functions names."
                          | otherwise = sequence (checkFunctionType <$> concat (M.elems fM)) >> pure funcMap
  where
    forMap = concatMap (\(fN, fs) -> fmap (\(Func fArgs t _) -> first ((,) fN) $ typeToKey fArgs t) fs) $ M.toList fM

    funcMap :: FuncMap
    funcMap = M.fromList forMap

    typeToKey :: [FuncArg] -> Type -> ([(Type, Maybe DataConstructor)], Type)
    typeToKey fArgs fType = (zip argsT dCs, resT)
      where
        (argsT, resT) = splitToArgsAndRes fType

        dCs = fmap toDC fArgs

        toDC :: FuncArg -> Maybe DataConstructor
        toDC VarArg{}          = Nothing
        toDC (PatternArg dc _) = Just dc

    checkFunctionType :: Func -> Either String ()
    checkFunctionType (Func fArgs fType fBody) = do
        when (any (`notElem` (fmap fst $ M.elems constM)) $ typeInArgs fType) $ Left $ "Unknown type in " <> show fType <> "."
        when (any isArrow argsT) (Left "Arguments of function can't be functions.")

        varM <- argsToVars fArgs argsT
        t    <- fst <$> evalStateT (inferExpressionType fBody) (varM, (constM, funcMap))

        if t == resT
          then pure ()
          else Left "Wrong function return type"
      where
        (argsT, resT) = splitToArgsAndRes fType

        typeInArgs :: Type -> [Type]
        typeInArgs (Arrow l r)     = typeInArgs l <> typeInArgs r
        typeInArgs (dt@DataType{}) = [dt]
        typeInArgs _               = []

        isArrow :: Type -> Bool
        isArrow (Arrow _ _) = True
        isArrow _           = False

    splitToArgsAndRes :: Type -> ([Type], Type)
    splitToArgsAndRes (Arrow l r) = first (l :) $ splitToArgsAndRes r
    splitToArgsAndRes t           = ([], t)

    argsToVars :: [FuncArg] -> [Type] -> Either String (Map VarName Type)
    argsToVars fArgs argsT | length fArgs /= length argsT = Left "Wrong number of arguments for function."
                           | otherwise = M.fromList . concat <$> sequence (zipWith mapArg fArgs argsT)
      where
        mapArg :: FuncArg -> Type -> Either String [(VarName, Type)]
        mapArg (VarArg x) t           = Right [(fromPVar x, t)]
        mapArg (PatternArg dc vars) t = do
          (t', argTypes) <- runIdentityT $ findDataConstructor constM dc

          when (t /= t') $ Left $ "Wrong constructor for type " <> show t <> "."
          when (length argTypes /= length vars) $ Left $ "Wrong number of arguments for pattern " <> show dc <> "."

          pure $ zip (fmap fromPVar vars) argTypes


inferExpressionType :: Expression -> InferState (Type, Maybe DataConstructor)
inferExpressionType e@(ITE cond th el) = do
    condT <- inferExpressionType cond
    thT   <- inferExpressionType th
    elT   <- inferExpressionType el

    if fst condT == Bool && thT == elT
      then pure thT
      else inferFail e
inferExpressionType (LetVar (PVar x) val ex) = do
    (valT, _) <- inferExpressionType val
    modify (first (M.insert x valT))
    inferExpressionType ex
inferExpressionType e'@(LetData dc vars e ex) = do
    (constM, _) <- fmap snd get

    dcT <- fst <$> findDataConstructor constM dc
    eT  <- inferExpressionType e

    if ((dcT, pure dc) == eT || snd eT == Nothing && dcT == fst eT) && length vars == length (snd $ constM M.! dc)
      then do
          modify (first (M.union $ M.fromList $ zipWith (,) (fmap fromPVar vars) $ snd $ constM M.! dc))
          inferExpressionType ex
      else inferFail e'
inferExpressionType (Primary p) = inferPrimary p
inferExpressionType (ArEx ae)   = inferArExpression ae

inferPrimary :: Primary -> InferState (Type, Maybe DataConstructor)
inferPrimary (PInt _)          = pure (Int, Nothing)
inferPrimary (PBool _)         = pure (Bool, Nothing)
inferPrimary (PDirected _ _)   = pure (Directed, Nothing)
inferPrimary (PUndirected _ _) = pure (Undirected, Nothing)
inferPrimary (PData dc args)   = do
    (constM, _) <- fmap snd get

    (dcT, dcArgsT) <- findDataConstructor constM dc
    argsT          <- fmap (fmap fst) $ sequence $ fmap inferExpressionType args

    if dcArgsT == argsT
      then pure (dcT, pure dc)
      else lift $ Left $ "Invalid arguments for " <> dc
inferPrimary (PVar var)   = do
    varM <- fmap fst get

    maybe (lift $ Left $ "No scuh variable: " <> var) (lift . pure . flip (,) Nothing) $ var `M.lookup` varM
inferPrimary (PFuncCall fName args) = do
    (_, funcM) <- fmap snd get

    argsT      <- sequence $ fmap inferExpressionType args
    let errorM = lift $ Left $ "No such function: " <> fName <> " with args of types " <> show argsT

    let keyVariantsI = zip [0..] $ replicate (length argsT + 1) argsT
    let keyVariants  = fmap (\(i, l) -> fmap (fmap (const Nothing)) (take i l) <> drop i l) keyVariantsI
    let lookedUp     = foldl1 (<|>) $ fmap ((`M.lookup` funcM) . (,) fName) keyVariants

    maybe errorM (lift . pure . flip (,) Nothing) lookedUp

inferArExpression :: ArExpression -> InferState (Type, Maybe DataConstructor)
inferArExpression (BinOp op e1 e2) = do
    e1T <- fst <$> inferExpressionType e1
    e2T <- fst <$> inferExpressionType e2

    let errorM = "Different types types in binop " <> show op <> ": " <> show e1T <> " and " <> show e2T <> "."
    when (e1T /= e2T) $ lift $ Left errorM

    lift $ inferOp op e1T
  where
    inferOp :: Operator -> Type -> Either String (Type, Maybe DataConstructor)
    inferOp op t | op `elem` [Eq, Neq]                                = Right (Bool, Nothing)
                 | op `elem` [Conj, Disj] && t == Bool                = Right (Bool, Nothing)
                 | op `elem` [Sum, Minus, Div, Mul, Pow] && t /= Bool = Right (t, Nothing)
                 | op `elem` [Lt, Gt, Le, Ge] && t /= Bool            = Right (Bool, Nothing)
                 | otherwise                                          = Left errorM
      where
        errorM = "Unknown binop " <> show op <> " for type " <> show t <> "."
inferArExpression (UnOp op e1) = do
    e1T <- fst <$> inferExpressionType e1

    case op of
      Neg    | e1T == Int  -> pure (Int, Nothing)
      LogNeg | e1T == Bool -> pure (Bool, Nothing)
      _      -> lift $ Left $ "Unknown unop " <> show op <> " for type " <> show e1 <> "."

fromPVar :: Primary -> VarName
fromPVar (PVar x) = x
fromPVar _        = error "fromPVar not on PVar."

inferFail :: MonadTrans m => Expression -> m (Either String) a
inferFail = lift . Left . ("Can't infer type of: " <>) . show

findDataConstructor :: MonadTrans m => ConstructorsMap -> DataConstructor -> m (Either String) (Type, [Type])
findDataConstructor m dc = maybe (lift $ Left $ "Constructor not found: " <> dc) (lift . pure) $ dc `M.lookup` m


