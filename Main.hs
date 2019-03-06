module Main where

import           Automaton
import           Combinators
import           Control.Applicative (many, some, (<|>))
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
  testErrors

  fileNames <- getArgs
  mapM_
    (\fileName -> do
        input <- readFile fileName
        let a = parseAutomaton input
        putStrLn $ printf "Parsing %s\n" fileName
        putStrLn $ either (printf "Not an automaton!\n%s") automatonInfo a
        putStrLn ""
    )
    fileNames

testError :: Eq ok => ParserS ok -> String -> BundleOfErrors String -> IO ()
testError p s errors = if parse p s == Left errors then putStrLn $ "OK: " ++ s
                       else putStrLn $ "FAIL: " ++ s

testErrors :: IO ()
testErrors = do
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
