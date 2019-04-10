module Main where

import           Automaton
import           Combinators
import           Control.Applicative (many, some, (<|>))
import           Expression
import           System.Environment
import           Text.Printf
import           Tokenizer

automatonInfo :: Automaton String String -> String
automatonInfo auto =
  let [dfa, nfa, complete, minimal] = map (\f -> if f auto then "yes" else "no") [isDFA, isNFA, isComplete, isMinimal] in
  printf "Hurray! It's an automaton!\nDeterministic:    %s\nNondeterministic: %s\nComplete:         %s\nMinimal:          %s" dfa nfa complete minimal

main :: IO ()
main = do
  runTokTests
  runTestErrors
  runTestMinimality
  runTestDeterminization
  runTestEpsClojure

  runTestExpressions

  fileNames <- getArgs
  mapM_
    (\fileName -> do
        input <- readFile fileName
        let a = parseExpression input
        let r = executeExpression input
        putStrLn $ printf "Parsing %s\n" fileName
        putStrLn $ either id show a
        putStrLn $ either id show r
        putStrLn ""
    )
    fileNames

testExpression :: String -> String -> IO ()
testExpression s s' = if fmap show (parseExpression s) == Right s' then putStrLn $ "OK: " ++ s
                     else putStrLn $ "FAIL: " ++ s

testExecuteExpression :: String -> Int -> IO ()
testExecuteExpression s s' = if executeExpression s == Right s' then putStrLn $ "OK: " ++ s
                             else putStrLn $ "FAIL: " ++ s

runTestExpressions :: IO ()
runTestExpressions = do
    putStrLn "Test: expressions."

    let expressionsTrivial =  "+" ++ "\n" ++
                              "|_-" ++ "\n" ++
                              "| |_+" ++ "\n" ++
                              "| | |_+" ++ "\n" ++
                              "| | | |_2" ++ "\n" ++
                              "| | | |_3" ++ "\n" ++
                              "| | |_+" ++ "\n" ++
                              "| | | |_4" ++ "\n" ++
                              "| | | |_10" ++ "\n" ++
                              "| |_12" ++ "\n" ++
                              "|_+" ++ "\n" ++
                              "| |_1" ++ "\n" ++
                              "| |_2"

    testExpression "2 + 3 + (4 + 10) - 12 + (1 + (2))" expressionsTrivial
    testExecuteExpression "2 + 3 + (4 + 10) - 12 + (1 + (2))" 10

    let expressionsHard = "==" ++ "\n" ++
                          "|_+" ++ "\n" ++
                          "| |_1" ++ "\n" ++
                          "| |_^" ++ "\n" ++
                          "| | |_2" ++ "\n" ++
                          "| | |_7" ++ "\n" ++
                          "|_-" ++ "\n" ++
                          "| |_3" ++ "\n" ++
                          "| |_+" ++ "\n" ++
                          "| | |_*" ++ "\n" ++
                          "| | | |_/" ++ "\n" ++
                          "| | | | |_11" ++ "\n" ++
                          "| | | | |_10" ++ "\n" ++
                          "| | | |_8" ++ "\n" ++
                          "| | |_1"

    testExpression "1 + 2 ^ 7 == 3 - (11 / 10 * 8 + 1)" expressionsHard
    testExecuteExpression "1 + 2 ^ 7 - 135 == 3 - (11 / 10 * 8 + 1)" 1

    let expressionLong = "||" ++ "\n" ++
                         "|_+" ++ "\n" ++
                         "| |_1" ++ "\n" ++
                         "| |_1" ++ "\n" ++
                         "|_<" ++ "\n" ++
                         "| |_^" ++ "\n" ++
                         "| | |_5" ++ "\n" ++
                         "| | |_3" ++ "\n" ++
                         "| |_+" ++ "\n" ++
                         "| | |_2" ++ "\n" ++
                         "| | |_==" ++ "\n" ++
                         "| | | |_1" ++ "\n" ++
                         "| | | |_+" ++ "\n" ++
                         "| | | | |_^" ++ "\n" ++
                         "| | | | | |_2" ++ "\n" ++
                         "| | | | | |_7" ++ "\n" ++
                         "| | | | |_==" ++ "\n" ++
                         "| | | | | |_1" ++ "\n" ++
                         "| | | | | |_2"
    testExpression "(((((((((((((((((((((((((((((((((1 + 1))))))))))))))))))))))))))))))))) || ((5^3)) < ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((2 + (1 == (((((((((((((((((((((((((((((((((2 ^ 7 + (((((((((((((((((((((((((((((((((1 == 2)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))" expressionLong

testEpsClojure :: String -> String -> IO ()
testEpsClojure s s' | Right a <- fmap epsClojure (parseAutomaton s), Right b <- parseAutomaton s', a == b = putStrLn $ "OK: " ++ s
                    | otherwise = putStrLn $ "FAIL: " ++ s

runTestEpsClojure :: IO ()
runTestEpsClojure = do
    putStrLn "Test: epsilon clojure."

    testEpsClojure "<<a, b>, <1, 2>, <1>, <2>, <(1, \\epsilon, 1), (1, \\epsilon, 2), (1, \\epsilon, 1), (2, \\epsilon, 2), (2, \\epsilon, 1)>>" "<<a, b>, <1&2>, <1&2>, <1&2>, <>>"
    testEpsClojure "<<a, b>, <1, 2, 3, 4>, <1>, <4>, <(1, a, 2), (1, \\epsilon, 4), (4, b, 2), (2, \\epsilon, 3), (3, \\epsilon, 2), (2, b, 3), (3, a, 3)>>" "<<a, b>, <1&4, 2&3>, <1&4>, <1&4>, <(1&4, a, 2&3), (1&4, b, 2&3), (2&3, b, 2&3), (2&3, a, 2&3)>>"

testDeterminization :: String -> String -> IO ()
testDeterminization s s' | Right a <- fmap determinize (parseAutomaton s), Right b <- parseAutomaton s', a == b = putStrLn $ "OK: " ++ s
                         | otherwise = putStrLn $ "FAIL: " ++ s

runTestDeterminization :: IO ()
runTestDeterminization = do
    putStrLn "Test: determinization."

    testDeterminization "<<a, b>, <1, 2>, <1>, <2>, <(1, a, 1), (1, a, 2), (1, b, 1), (2, b, 2), (2, b, 1)>>" "<<a, b>, <1, 1&2>, <1>, <1&2>, <(1, b, 1), (1, a, 1&2), (1&2, a, 1&2), (1&2, b, 1&2)>>"

testMinimality :: String -> Bool -> IO ()
testMinimality s b = if fmap isMinimal (parseAutomaton s) == Right b then putStrLn $ "OK: " ++ s
                     else putStrLn $ "FAIL: " ++ s

runTestMinimality :: IO ()
runTestMinimality = do
    putStrLn "Test: minimality."

    let autoEx = "<<0, 1>  , <a, b, c, d, e, f, g> , <a>, <f, g>, <(a, 1, b), (b, 1, a), (b, 0, c), (a, 0, c), (c, 0, d), (c, 1, d), (d, 1, f), (d, 0, e), (e, 0, f), (e, 1, g), (g, 0, g), (g, 1, f), (f, 0, f), (f, 1,f)>>"
    testMinimality autoEx False

    let autoTrivial = "<<0, 1>, <a, b>, <a>, <b>, <(a, 1, b)>>"
    testMinimality autoTrivial True

    let autoHard = "<\n"  ++
                   "<a,b,c,d>,\n" ++
                   "<0,1,2,3,4,5>,\n" ++
                   "<0>,\n" ++
                   "<3,4,5>,\n" ++
                   "<(0, a, 1), (0, b, 2), (0, d, 2), (0, c, 3), (1, a, 4), (2, b, 4), (2, a, 5), (3, a, 4)>\n" ++
                   ">"
    testMinimality autoHard False


testError :: Eq ok => ParserS ok -> String -> BundleOfErrors String -> IO ()
testError p s errors = if parse p s == Left errors then putStrLn $ "OK: " ++ s
                       else putStrLn $ "FAIL: " ++ s

runTestErrors :: IO ()
runTestErrors = do
    putStrLn "Test: errors."

    testError (many space *> (char 'a' <|> char 'b')) "            c" [ ParserError (Just (0, 12)) "Token doesn't satisfy condition."
                                                                      , ParserError (Just (0, 12)) "Token doesn't satisfy condition."
                                                                      ]

    testError (char 'a' *> char '\n' *> char '-' *> char 'c') "a\n-d--\nb\nc" [ ParserError (Just (1, 1)) "Token doesn't satisfy condition."
                                                                              ]

    testError (some (anyChar *> char '\n') *> char 'd') "a\nd\nb\nc" [ ParserError (Just (3, 0)) "Token doesn't satisfy condition."
                                                                     ]

    testError (some (anyChar *> char '\n') *> char 'b' *> many digit *> eof) "a\nd\nb222\n" [ ParserError (Just (2, 4)) "Can't parse EOF."
                                                                                            ]

testParser :: String -> [Tok] -> IO ()
testParser s tokens = if tokenize s == Right tokens then putStrLn $ "OK: " ++ s
                      else putStrLn $ "FAIL: " ++ s

runTokTests :: IO ()
runTokTests = do
   putStrLn "Test: Parsing identifires."

   testParser "xax Asw2 a2222q AAA_ _1" [Ident "xax", Ident "Asw2", Ident "a2222q", Ident "AAA_", Ident "_1"]
   testParser "Class python3_ " [Ident "Class", Ident "python3_", EOF]
   testParser "Haskell i_s thE   BeSt LanGUage     _ " [ Ident "Haskell", Ident "i_s", Ident "thE", Ident "BeSt"
                                                       , Ident "LanGUage", Ident "_", EOF
                                                       ]

   putStrLn ""

   putStrLn "Test: Parsing keywords."

   testParser "for if   in True  " [KeyWord "for", KeyWord "if", KeyWord "in", KeyWord "True", EOF]
   testParser "  else if  False in  " [KeyWord "else", KeyWord "if", KeyWord "False", KeyWord "in", EOF]

   putStrLn ""

   putStrLn "Test: Parsing numbers."

   testParser "00_0000_000_0_00 21_0000_32_1" [Number 0, Number 210000321]
   testParser "1 2 3   4 5 6    7_7 9" [Number 1, Number 2, Number 3, Number 4, Number 5, Number 6, Number 77, Number 9]

   putStrLn ""

   putStrLn "Test: Parsing complex expressions."

   testParser "if Class Haskell lambda 777 then 0000_0" [ KeyWord "if", Ident "Class", Ident "Haskell"
                                                        , KeyWord "lambda", Number 777, Ident "then"
                                                        , Number 0
                                                        ]
   testParser "then else    ___1 in 818181_0000   " [ Ident "then", KeyWord "else", Ident "___1", KeyWord "in"
                                                    , Number 8181810000, EOF
                                                    ]
