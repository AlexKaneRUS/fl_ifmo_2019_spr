module Main where

import           Automaton
import           Combinators
import           Control.Applicative (many, some, (<|>))
import           Control.Monad       (join)
import           Data.Bifunctor      (first)
import qualified Data.Map.Strict     as M
import           Gradskell
import           System.Environment
import           Text.Printf
import           Tokenizer

main :: IO ()
main = do
  fileNames <- getArgs
  mapM_
    (\fileName -> do
        input <- readFile fileName
        let a = undefined
        putStrLn $ printf "Parsing %s\n" fileName
        putStrLn ""
    )
    fileNames

  parserTests
  putStrLn ""
  typeInfererTests

testParser :: Eq a => ParserS a -> String -> a -> IO ()
testParser p s ast = if parse (p <* eof) s == Right ast then putStrLn $ "OK: " ++ s
                     else putStrLn $ "FAIL: " ++ s

parserTests :: IO ()
parserTests = do
    putStrLn "Parser tests.\n"

    let letData = LetData "Tuple" [PVar "x", PVar "y"] (Primary (PData "Tuple" [Primary (PInt 1),Primary (PInt 2)])) (ArEx (BinOp Sum (Primary (PVar "x")) (Primary (PVar "y"))))
    testParser expressionP "let Tuple x y  = Tuple 1 2 in x + y" letData

    let letVarLetVar = LetVar (PVar "graphA") (Primary (PDirected [1,2,3] [(1,2,0),(2,0,0)])) (LetVar (PVar "graphB") (Primary (PDirected [1,2,3] [(1,2,0),(2,0,0)])) (ITE (Primary (PBool True)) (Primary (PBool False)) (ArEx (BinOp Lt (Primary (PVar "graphA")) (Primary (PVar "graphB"))))))
    testParser expressionP "let graphA = <[1, 2, 3] , [(1, 2, 0) , (2, 0, 0)]> in let graphB = <[1, 2, 3] , [(1, 2, 0) , (2, 0, 0)]> in if True then False else graphA < graphB" letVarLetVar

    let notLt = Primary (PData "PairU" [Primary (PDirected [1] []),Primary (PUndirected [0] [])])
    testParser expressionP "(PairU <[1], []> <<[0], []>>)" notLt

    let funcWithMultipleArguments = Primary $ PFuncCall "func" [Primary (PData "PairD" [Primary (PDirected [1] []),Primary (PDirected [2] [])]),Primary (PData "PairU" [Primary (PVar "graphA"),Primary (PVar "graphB")]),Primary (PInt 0)]
    testParser expressionP "func(PairD <[1], []> <[2], []>, PairU graphA graphB, 0)" funcWithMultipleArguments

    let simpleType = Int
    testParser typeP "Int" simpleType

    let hardType = Arrow (Arrow Int Int) (Arrow Bool (Arrow Directed (Arrow Undirected (DataType "MyType"))))
    testParser typeP "(       Int     -> Int ) -> Bool    -> (Directed -> Undirected -> (MyType))" hardType

    let hardADT = ("MyType",[Constructor "AAA" [Bool],Constructor "BBBB" [Int,Bool,Arrow Int (Arrow Bool Undirected)],Constructor "CCCC" [],Constructor "EEEE" [Arrow (DataType "A") (Arrow (DataType "B") (DataType "C"))]])
    testParser aDTP "data MyType = AAA Bool |   BBBB Int Bool (Int -> Bool -> Undirected)| CCCC | EEEE (A -> B -> C)" hardADT

    let simpleFunc = ("f",Func [VarArg (PVar "x"),VarArg (PVar "y")] (Arrow Int (Arrow Bool Int)) (ArEx (BinOp Gt (ArEx (BinOp Sum (Primary (PVar "x")) (Primary (PVar "y")))) (Primary (PInt 0)))))
    testParser functionP "f(x, y): Int -> Bool -> Int = { x + y > 0 }" simpleFunc

    let noArgsFunc = ("f",Func [] (Arrow Int (Arrow Bool (Arrow Int (Arrow (Arrow (DataType "A") (DataType "B")) (DataType "C"))))) (LetVar (PVar "x") (Primary (PInt 10)) (ArEx (BinOp Div (Primary (PVar "x")) (Primary (PVar "x"))))))
    testParser functionP "f(): Int -> Bool -> Int -> (A -> B) -> C = { let x = 10 in x / x }" noArgsFunc

    let patternMatchInArgs = ("myCoolFunction",Func [PatternArg "MyType" [PVar "x",PVar "y"],VarArg (PVar "a"),PatternArg "MyT" [PVar "a",PVar "b",PVar "c",PVar "d",PVar "e"]] (Arrow (DataType "MyType") (Arrow (DataType "A") (Arrow (Arrow (DataType "A") (DataType "B")) (DataType "C")))) (LetVar (PVar "x") (Primary (PInt 10)) (LetData "TT" [PVar "a"] (ArEx (BinOp Sum (Primary (PVar "a")) (Primary (PVar "b")))) (ArEx (BinOp Minus (Primary (PVar "x")) (Primary (PVar "y")))))))
    testParser functionP "myCoolFunction(MyType x y, a, MyT a b c d e): MyType -> A -> (A -> B) -> C = { let x = 10 in let TT a = a + b in x - y }" patternMatchInArgs

    let program = GradskellProgram (M.fromList [("Pair",[Constructor "PairU" [Undirected,Undirected],Constructor "PairD" [Directed,Directed]])]) (M.fromList [("min",[Func [PatternArg "PairU" [PVar "graphA",PVar "graphB"]] (Arrow (DataType "Pair") Bool) (ITE (ArEx (BinOp Lt (Primary (PVar "graphA")) (Primary (PVar "graphB")))) (Primary (PVar "graphA")) (Primary (PVar "graphB"))),Func [PatternArg "PairD" [PVar "graphA",PVar "graphB"]] (Arrow (DataType "Pair") Bool) (ITE (ArEx (BinOp Lt (Primary (PVar "graphA")) (Primary (PVar "graphB")))) (Primary (PVar "graphA")) (Primary (PVar "graphB")))])]) (LetVar (PVar "graphA") (Primary (PDirected [1,2,3] [(1,2,0),(2,0,0)])) (LetVar (PVar "graphB") (Primary (PDirected [1,2] [(1,0,0)])) (Primary (PFuncCall "min" [Primary (PData "PairU" [Primary (PVar "graphA"),Primary (PVar "graphB")])]))))
    programS    <- readFile "test/program.gs"
    testParser gradskellP programS program

    let program1 = GradskellProgram {dataTypes = M.fromList [("C",[Constructor "C" []]),("List",[Constructor "Nil" [],Constructor "Cons" [DataType "Pair",DataType "List"]]),("Pair",[Constructor "PairU" [Undirected,Undirected],Constructor "PairD" [Directed,Directed]]),("Pair2",[Constructor "Pair2U" [Undirected,Undirected],Constructor "PairDU" [Directed,Directed]])], functions = M.fromList [], program = LetVar (PVar "graphA") (Primary (PDirected [1,2,3] [(1,2,0),(2,0,0)])) (LetVar (PVar "graphB") (Primary (PDirected [1,2] [(1,0,0)])) (Primary (PData "C" [])))}
    programS    <- readFile "test/program1.gs"
    testParser gradskellP programS program1

testInferer :: String -> String -> Type -> IO ()
testInferer s s' t = if (join $ inferTypeForGradskellAst <$> first (const []) (parse (gradskellP <* eof) s)) == Right t then putStrLn $ "OK: " ++ s'
                  else putStrLn $ "FAIL: " ++ s'

typeInfererTests :: IO ()
typeInfererTests = do
    putStrLn "Inferer test.\n"

    program2 <- readFile "test/program2.gs"
    testInferer program2 "Simple gradskell program." Undirected

    program3 <- readFile "test/program3.gs"
    testInferer program3 "Hard function named func." Bool

    program4 <- readFile "test/program4.gs"
    testInferer program3 "Functions called from functions, zero arg function, choice of overriding function." Bool

