{-# LANGUAGE RecordWildCards #-}

module Gradskell where

import           Combinators                  hiding (error)
import           Control.Applicative          (many, some, (<|>))
import           Control.Monad                (when, zipWithM)
import           Control.Monad.State
import           Control.Monad.Trans.Class    (MonadTrans, lift)
import           Control.Monad.Trans.Identity (runIdentityT)
import           Data.Bifunctor               (bimap, first, second)
import           Data.Char                    (isAlphaNum, isDigit, isLower,
                                               isUpper)
import           Data.Either                  (isRight, lefts)
import           Data.Function                (on)
import           Data.List                    (groupBy, nub, nubBy, sortOn)
import qualified Data.List                    as L
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as M
import           Data.Semigroup               ((<>))
import           Text.Printf

import           Debug.Trace                  (trace)

--------------------------------------------------------------------------------
-- Gradskell AST.
--------------------------------------------------------------------------------

data GradskellAst = GradskellProgram { dataTypes :: Map Type [Constructor]
                                     , functions :: Map FuncName [Func]
                                     , program   :: Expression
                                     }
  deriving (Eq, Show)

type DataConstructor = String
data Constructor = Constructor { constructor :: DataConstructor
                               , argTypes    :: [Type]
                               }
  deriving (Eq, Show)
type DataType = String
data Type = Int | Bool | Directed | Undirected | DataType DataType [Type] | Arrow Type Type | Unit | TVar VarName
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

type VarName = String
data Primary = PInt Int
             | PBool Bool
             | PDirected [Int] [(Int, Int, Int)]
             | PUndirected [Int] [(Int, Int, Int)]
             | PData DataConstructor [Expression]
             | PVar VarName
             | PFuncCall FuncName [Expression]
  deriving (Eq, Show)

data ArExpression = BinOp Operator Expression Expression
                  | UnOp Operator Expression
  deriving (Eq, Show)

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

--------------------------------------------------------------------------------
-- Gradskell Parser.
--------------------------------------------------------------------------------

gradskellP :: ParserS GradskellAst
gradskellP =  GradskellProgram
          <$> (M.fromList <$> many (many spaceOrCommentaryP *> aDTP))
          <*> (M.fromList . ungroups <$> many (many spaceOrCommentaryP *> functionP))
          <*> (many spaceOrCommentaryP *> expressionP)

aDTP :: ParserS (Type, [Constructor])
aDTP =  (,)
    <$> (string "data" *> betweenSpaces dataTypeP <* betweenSpaces (char '='))
    <*> ((++) <$> many (constructorP <* betweenSpaces (char '|')) <*> (fmap pure constructorP))
  where
    constructorP :: ParserS Constructor
    constructorP = Constructor <$> dataConstructorP <*> many (some spaceOrCommentaryP *> helper)

    helper :: ParserS Type
    helper = intP <|> boolP <|> directedP <|> undirectedP <|> dataTypePEmpty <|> tVarP' <|> unitP <|> betweenBrackets1 typeP

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

expressionP :: ParserS Expression
expressionP = betweenBrackets (iTEP <|> letVarP <|> letDataP <|> arExpressionP <|> primaryP)
  where
    iTEP :: ParserS Expression
    iTEP =  ITE
        <$> (string "if" *> some spaceOrCommentaryP *> expressionP)
        <*> (some spaceOrCommentaryP *> string "then" *> some spaceOrCommentaryP *> expressionP)
        <*> (some spaceOrCommentaryP *> string "else" *> some spaceOrCommentaryP *> expressionP)

    letVarP :: ParserS Expression
    letVarP =  LetVar
           <$> (string "let" *> some spaceOrCommentaryP *> pVarP)
           <*> (some spaceOrCommentaryP *> char '=' *> some spaceOrCommentaryP *> expressionP)
           <*> (some spaceOrCommentaryP *> string "in" *> some spaceOrCommentaryP *> expressionP)

    letDataP :: ParserS Expression
    letDataP =  uncurry LetData
            <$> (string "let" *> some spaceOrCommentaryP *> patternMatchDataP)
            <*> (betweenSpaces1 (char '=') *> expressionP)
            <*> (betweenSpaces1 (string "in") *> expressionP)

    arExpressionP :: ParserS Expression
    arExpressionP = expression exprOpsListAST primaryP betweenBrackets1

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

patternMatchDataP :: ParserS (DataConstructor, [Primary])
patternMatchDataP =  (,)
                 <$> dataConstructorP
                 <*> ((some spaceOrCommentaryP *> some (many spaceOrCommentaryP *> pVarP)) <|> pure [])

pVarP :: ParserS Primary
pVarP = fmap PVar varNameP

tVarP :: ParserS Type
tVarP = fmap TVar varNameP

tVarP' :: ParserS Type
tVarP' = do
  name <- varNameP

  nextSymbol <- peek

  if nextSymbol /= Just '('
    then pure $ TVar name
    else fail "This is not variable, but name of function."

primaryP :: ParserS Expression
primaryP = Primary <$> primaryP'
  where
    primaryP' :: ParserS Primary
    primaryP' = pIntP <|> pBoolP <|> pUndirectedP <|> pDirectedP <|> pDataP <|> pFuncCallP <|> pVarP

    pIntP :: ParserS Primary
    pIntP = fmap PInt int

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
        _ <- many spaceOrCommentaryP

        listOfVertices <- parseList int commaP lBracketP rBracketP 0

        when (null listOfVertices) (fail "Empty list of vertices for graph.")

        _ <- char ','
        _ <- many spaceOrCommentaryP

        listOfEdges <- parseList tripleP commaP lBracketP rBracketP 0

        _ <- many spaceOrCommentaryP
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
               <* many spaceOrCommentaryP <*> int <* many spaceOrCommentaryP <* char ','
               <* many spaceOrCommentaryP <*> int <* many spaceOrCommentaryP <* char ','
               <* many spaceOrCommentaryP <*> int <* many spaceOrCommentaryP <* char ')'

    pDataP :: ParserS Primary
    pDataP = PData <$> dataConstructorP <*> many (many spaceOrCommentaryP *> (primaryP <|> betweenBrackets1 expressionP))

    pFuncCallP :: ParserS Primary
    pFuncCallP =  PFuncCall
              <$> funcNameP
              <*> (betweenBrackets1 (((:) <$> expressionP <*> many (betweenSpaces (char ',') *> expressionP))
              <|> pure []))

varNameP :: ParserS VarName
varNameP = do
    name <- (:) <$> (char '_' <|> satisfy isLower) <*> many (char '_' <|> satisfy isAlphaNum)
    when (name `elem` keyWords) (fail "Variable name can't match any of keywords.")
    pure name

keyWords :: [String]
keyWords = [ "in", "if", "then", "else", "let", "data" ]

dataConstructorP :: ParserS DataConstructor
dataConstructorP = (:) <$> satisfy isUpper <*> many (satisfy isAlphaNum)

funcNameP :: ParserS FuncName
funcNameP = varNameP

dataTypePEmpty :: ParserS Type
dataTypePEmpty =  fmap (flip DataType []) dataConstructorP

dataTypeP :: ParserS Type
dataTypeP =  DataType <$> dataConstructorP <*> many (some spaceOrCommentaryP *> notBracketTP)

typeP :: ParserS Type
typeP = arrowP <|> intP <|> boolP <|> directedP <|> undirectedP <|> unitP <|> tVarP <|> dataTypeP

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

notBracketTP :: ParserS Type
notBracketTP = intP <|> boolP <|> directedP <|> undirectedP <|> dataTypePEmpty <|> tVarP <|> unitP <|> betweenBrackets1 typeP

arrowP :: ParserS Type
arrowP = foldr1 Arrow <$> ((:) <$> helper <*> some (betweenSpaces (string "->") *> helper))
  where
    helper :: ParserS Type
    helper = intP <|> boolP <|> directedP <|> undirectedP <|> dataTypeP <|> tVarP <|> unitP <|> betweenBrackets1 typeP

spaceOrCommentaryP :: ParserS String
spaceOrCommentaryP = (pure <$> space) <|> commentaryP

notNestedCommentaryP :: ParserS String
notNestedCommentaryP = do
    st <- openP
    t  <- textP'
    en <- closeP

    pure $ concat [st, t, en]
  where
    textP' :: ParserS String
    textP' = do
        ab <- peekN 2

        if ab == "-}"
          then pure ""
          else (:) <$> anyChar <*> textP'

commentaryP :: ParserS String
commentaryP = oneLineCommentaryP <|> manyLineCommentaryP
  where
    oneLineCommentaryP :: ParserS String
    oneLineCommentaryP = (++) <$> string "--" <*> many (satisfy (/= '\n'))

    manyLineCommentaryP :: ParserS String
    manyLineCommentaryP = do
        st <- openP
        t  <- textP <|> pure ""

        mlc <- manyLineCommentaryP <|> pure ""

        t'  <- textP <|> pure ""
        en <- closeP

        pure $ concat [st, t, mlc, t', en]
      where
        textP :: ParserS String
        textP = do
            ab <- peekN 2

            if ab == "{-" || ab == "-}"
              then pure ""
              else (:) <$> anyChar <*> textP

openP :: ParserS String
openP = string "{-"

closeP :: ParserS String
closeP = string "-}"

--------------------------------------------------------------------------------
-- Utility functions.
--------------------------------------------------------------------------------

ungroups :: (Eq a, Ord a) => [(a, b)] -> [(a, [b])]
ungroups = fmap (\x -> (fst $ head x, fmap snd x)) . groupBy ((==) `on` fst) . sortOn fst

betweenSpaces :: ParserS a -> ParserS a
betweenSpaces = between (concat <$> many spaceOrCommentaryP) (concat <$> many spaceOrCommentaryP)

betweenSpaces1 :: ParserS a -> ParserS a
betweenSpaces1 = between (concat <$> some spaceOrCommentaryP) (concat <$> some spaceOrCommentaryP)

betweenBrackets :: ParserS a -> ParserS a
betweenBrackets p = do
    _        <- many spaceOrCommentaryP
    bracketM <- peek

    case bracketM of
      Just '(' -> char '(' *> betweenBrackets (p <* many spaceOrCommentaryP <* char ')') <|> p
      _        -> p

betweenBrackets1 :: ParserS a -> ParserS a
betweenBrackets1 p = char '(' *> many spaceOrCommentaryP *> betweenBrackets (p <* many spaceOrCommentaryP <* char ')')

--------------------------------------------------------------------------------
-- Type checking.
--------------------------------------------------------------------------------

type ConstructorsMap = Map DataConstructor (Type, [Type])
type FuncMap         = Map (FuncName, [Type]) Type
type VarMap          = Map VarName Type
type InferState      = StateT ((VarMap, TypeContext), ((ConstructorsMap, FuncMap), [Int])) (Either String)
type TypeContext     = Map VarName Type

matchTypes :: Type -> Type -> Bool
matchTypes t1 t2 = isRight (execStateT (getSubstitution t1 t2) ((mempty, mempty), ((mempty, mempty), [1..])))

getSubstitution :: Type -> Type -> InferState ()
getSubstitution (DataType n l) (DataType n' l') | n == n' && length l == length l' = const () <$> zipWithM getSubstitution l l'
                                                | otherwise = lift . Left $ "Different length in data types: " <> n <> " and " <> n'
getSubstitution (Arrow l r) (Arrow l' r') = getSubstitution l l' >> getSubstitution r r'
getSubstitution x y | not (isTVar x) && isTVar y = lift . Left $ "Can't match " <> show x <> " with " <> show y
                    | TVar x' <- x               = do
                        ((_, m), _) <- get

                        let tVal = x' `M.lookup` m
                        when (checkVal tVal) (lift . Left $ "Dublicating value for type var " <> show x)

                        modify (first $ second $ fmap (substitute x y))
                        modify (first $ second $ M.insert x' y)
                        allSubsInVars
                    | x == y              = pure ()
                    | otherwise           = lift . Left $ "Different types: " <> show x <> " and " <> show y
  where
    checkVal :: Maybe Type -> Bool
    checkVal (Just x') = not (isTVar x') && x' /= y
    checkVal _         = False

substitute :: Type -> Type -> Type -> Type
substitute tgt y (DataType n l)     = DataType n $ fmap (substitute tgt y) l
substitute tgt y (Arrow l r)        = Arrow (substitute tgt y l) (substitute tgt y r)
substitute tgt@(TVar x) y (TVar x') | x == x'   = y
                                    | otherwise = TVar x'
substitute _ _ t                    = t

isTVar :: Type -> Bool
isTVar TVar{} = True
isTVar _      = False

fromTVar :: Type -> Bool
fromTVar TVar{} = True
fromTVar _      = False

allSubsInVars :: InferState ()
allSubsInVars = do
    (_, tM) <- fmap fst get
    modify (first $ first $ fmap (subType tM))

inferTypeForGradskellAst :: GradskellAst -> Either [String] Type
inferTypeForGradskellAst GradskellProgram{..} = do
    constM <- getConstructorsMap dataTypes
    funcM  <- first pure $ getFunctionsMap constM functions

    bimap pure fst $ evalStateT (inferExpressionType program) ((mempty, mempty), ((constM, funcM), [1..]))

getConstructorsMap :: Map Type [Constructor] -> Either [String] ConstructorsMap
getConstructorsMap dataTypesM | null errorList                         = pure res
                              | nubBy ((==) `on` fst) forMap /= forMap = Left ["Duplicating constructor names."]
                              | otherwise                              = Left errorList
  where
    dataTypes = M.keys dataTypesM

    checkedDataTypes = M.elems $ M.mapWithKey checkConstructors dataTypesM
    errorList        = lefts checkedDataTypes

    forMap              = concatMap (\(t, l) -> fmap (\x -> (constructor x, (t, argTypes x))) l) $ M.toList dataTypesM
    res                 = M.fromList forMap

    checkConstructors :: Type -> [Constructor] -> Either String ()
    checkConstructors dt = eConcat . fmap checkConstructor
      where
        checkConstructor :: Constructor -> Either String ()
        checkConstructor (Constructor co types) = eConcat $ fmap checkType types
          where
            checkType :: Type -> Either String ()
            checkType (dt'@DataType{}) | allVars dt `L.union` allVars dt' /= allVars dt = Left dtError'
                                       | any (\dt'' -> matchTypes dt'' dt') dataTypes   = pure ()
                                       | otherwise                                      = Left dtError
              where
                dtError  = "Data type " <> show dt <> ", constructor " <> co <> ": No such data type " <> show dt' <> "."
                dtError' = "Data type " <> show dt <> ", constructor " <> co <> ": Some of type variables do not exist " <> show (allVars dt') <> "."
            checkType (Arrow l r) | Right _ <- checkType l = checkType r
                                  | otherwise              = checkType l
            checkType _           = pure ()

freshenConst :: (Type, [Type]) -> InferState (Type, [Type])
freshenConst (t, ts) = do
    ns             <- fmap (snd . snd) get
    let (t' : ts', ns') = freshVariablesForTypes ns $ t : ts
    modify (second $ second $ const ns')
    pure (t', ts')

eConcat :: [Either String ()] -> Either String ()
eConcat = fmap (const ()) . sequence

subType :: TypeContext -> Type -> Type
subType m t = foldl (\t' (x, x') -> substitute (TVar x) x' t') t $ M.toList m

getFunctionsMap :: Map DataConstructor (Type, [Type]) -> Map FuncName [Func] -> InferState FuncMap
getFunctionsMap constM fM | nubBy ((==) `on` fst) forMap /= forMap = Left "Duplicating functions names."
                          | noOverrideOnReturnTypeCheck            = Left "No overriding on return type allowed."
                          | otherwise = sequence (checkFunctionType <$> funcs) >> pure funcMap
  where
    forMap = concatMap (\(fN, fs) -> fmap (\(Func fArgs t _) -> first ((,) fN) $ typeToKey fArgs t) fs) $ M.toList fM

    funcs = transformFTs $ concat $ M.elems fM

    noOverrideOnReturnTypeCheck = any ((> 1) . length)
                                $ fmap (nubBy ((==) `on` (\(_, _, z) -> z)))
                                $ groupBy ((==) `on` (\(x, y, z) -> (x, y)))
                                $ sortOn (\(x, y, z) -> (x, y))
                                $ fmap (\(x, y) -> (fst x, fmap fst (snd x), y)) forMap

    dataTypes = fmap fst $ M.elems constM

    freshenFunc :: Func -> InferState Func
    freshenFunc (Func fArgs fType fBody) = do
        ns             <- fmap (snd . snd) get
        let (fType', ns') = freshVariablesForTypes ns fType
        modify (second $ second $ const ns')
        pure (Func fArgs fType' fBody)

    funcMap :: FuncMap
    funcMap = M.fromList $ fmap (first (fmap (fmap fst))) forMap

    typeToKey :: [FuncArg] -> Type -> ([(Type, Maybe DataConstructor)], Type)
    typeToKey fArgs fType = (zip argsT dCs, resT)
      where
        (argsT, resT) = splitToArgsAndRes fType

        dCs = fmap toDC fArgs

        toDC :: FuncArg -> Maybe DataConstructor
        toDC VarArg{}          = Nothing
        toDC (PatternArg dc _) = Just dc

    checkFunctionType :: Func -> InferState ()
    checkFunctionType (Func fArgs fType fBody) = do
        when (any (not . typeExists) $ typeInArgs fType) $ Left $ "Unknown type in " <> show fType <> "."
        when (any isArrow argsT) (Left "Arguments of function can't be functions.")

        varM                  <- argsToVars fArgs argsT
        ((t, _), ((_, m), _)) <- runStateT (inferExpressionType fBody) ((varM, mempty), (constM, funcMap))

        if matchTypes t resT
          then pure ()
          else Left "Wrong function return type"
      where
        (argsT, resT) = splitToArgsAndRes fType

        typeExists :: Type -> Bool
        typeExists t = t `elem` dataTypes || any (flip matchTypes t) dataTypes

        isArrow :: Type -> Bool
        isArrow (Arrow _ _) = True
        isArrow _           = False

    typeInArgs :: Type -> [Type]
    typeInArgs (Arrow l r)     = typeInArgs l <> typeInArgs r
    typeInArgs (dt@DataType{}) = [dt]
    typeInArgs _               = []

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
          (t', argTypes) <- fmap freshenConst $ runIdentityT $ findDataConstructor constM dc

          when (not $ matchTypes t' t) $ Left $ "Wrong constructor for type " <> show t <> "."
          when (length argTypes /= length vars) $ Left $ "Wrong number of arguments for pattern " <> show dc <> "."

          pure $ zip (fmap fromPVar vars) argTypes

freshVariablesForTypes :: [Int] -> [Type] -> ([Type], [Int])
freshVariablesForTypes inds tss = helper tss inds
  where
    helper :: [Type] -> State [Int] [Type]
    helper ts = do
        nsl <- get

        let allVs = nub $ concatMap allVars ts

        let ns = take (length allVs) nsl
        modify (drop (length allVs))

        let newVars = zipWith (<>) (repeat "x") (fmap show ns)
        let subs    = zipWith (\v nv -> (v, TVar nv)) allVs newVars

        pure $ fmap (subType (M.fromList subs)) ts

allVars :: Type -> [VarName]
allVars (DataType n l) = nub $ concatMap allVars l
allVars (Arrow l r)    = nub $ allVars l ++ allVars r
allVars (TVar x)       = [x]
allVars _              = []


inferExpressionType :: Expression -> InferState (Type, Maybe DataConstructor)
inferExpressionType e@(ITE cond th el) = do
    condT <- inferExpressionType cond
    thT   <- inferExpressionType th
    elT   <- trace (show thT) $ inferExpressionType el

    if fst condT == Bool && fst thT == fst elT
      then pure thT
      else inferFail e
inferExpressionType (LetVar (PVar x) val ex) = do
    (valT, _) <- inferExpressionType val
    modify (first (first (M.insert x valT)))
    allSubsInVars
    inferExpressionType ex
inferExpressionType e'@(LetData dc vars e ex) = do
    (constM, _) <- fmap snd get

    (dcT, dcArgsT) <- freshenConst <$> findDataConstructor constM dc
    (eT, eDC)      <- inferExpressionType e

    sub <- getSubstitution dcT eT

    if (pure dc == eDC || null eDC) && length vars == length dcArgsT
      then do
          modify (first (first $ M.union $ M.fromList $ zipWith (,) (fmap fromPVar vars) $ dcArgsT))
          allSubsInVars
          inferExpressionType ex
      else inferFail e'
inferExpressionType (Primary p) = inferPrimary p
inferExpressionType (ArEx ae)   = inferArExpression ae

inferPrimary :: Primary -> InferState (Type, Maybe DataConstructor)
inferPrimary (PInt _)          = pure (Int, Nothing)
inferPrimary (PBool _)         = pure (Bool, Nothing)
inferPrimary (PDirected _ _)   = pure (Directed, Nothing)
inferPrimary (PUndirected _ _) = pure (Undirected, Nothing)
inferPrimary (PData dc args)   = inferStateLoc $ do
    ((_, tvarM), (constM, _)) <- get

    argsT          <- trace (show args) $ fmap (fmap fst) $ sequence $ fmap inferExpressionType args
    (dcT, dcArgsT) <- freshenConst <$> findDataConstructor constM dc

    if trace (show (dcArgsT, argsT)) $ length dcArgsT == length argsT
      then do
          zipWithM substitutionForZip dcArgsT argsT
          tvarM' <- fmap (snd . fst) get
          pure (subType tvarM' dcT, pure dc)
      else lift $ Left $ "Invalid arguments for " <> dc
inferPrimary (PVar var)   = do
    (varM, tvarM) <- fmap fst get

    case var `M.lookup` varM of
      Nothing -> lift $ Left $ "No scuh variable: " <> var
      Just x  -> pure $ (subType tvarM x, Nothing)
inferPrimary (PFuncCall fName args) = inferStateLoc $ do
    argsT <- fmap (fmap fst) $ sequence $ fmap inferExpressionType args
    tvarM <- fmap (snd . fst) get
    findFunction (fName, argsT)
  where
    findFunction :: (String, [Type]) -> InferState (Type, Maybe DataConstructor)
    findFunction (fName, argsT) = do
        funcM <- fmap (snd . snd) get

        let funcs  = M.toList funcM
        let foundM = L.find (\((n, ts), _) -> n == fName && length ts == length argsT && and (zipWith matchTypes ts argsT)) funcs

        case foundM of
          Nothing               -> lift $ Left $ "No such function: " <> fName <> " with args of types " <> show argsT
          Just ((_, ts), resT)  -> do
              zipWithM substitutionForZip ts argsT
              tvarM <- fmap (snd . fst) get
              pure (subType tvarM resT, Nothing)

substitutionForZip :: Type -> Type -> InferState ()
substitutionForZip t t' = do
    tvarM <- fmap (snd . fst) get
    getSubstitution t (subType tvarM t')

inferStateLoc :: InferState a -> InferState a
inferStateLoc is = do
    st <- get
    case runStateT is st of
      Left er   -> lift . Left $ er
      Right (res, st') -> do
          let ((_, tvarM), _) = st'

          modify (first $ first $ fmap (subType tvarM))
          pure res

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
