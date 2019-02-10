module Main where

import           Tokenizer

runTokenizer :: String -> IO ()
runTokenizer input = do
    putStrLn input
    putStrLn $ show $ tokenize input
    putStrLn ""

main :: IO ()
main = do
    runTokenizer " 1 2 abc if "
    runTokenizer " "

    runTests

testParser :: String -> [Token] -> IO ()
testParser s tokens = if tokenize s == tokens then putStrLn $ "OK: " ++ s
                      else putStrLn $ "FAIL: " ++ s

runTests :: IO ()
runTests = do
    putStrLn "Test: Parsing identifires."

    testParser "xax Asw2 a2222q AAA_ _1" [Ident "xax", Ident "Asw2", Ident "a2222q", Ident "AAA_", Ident "_1"]
    testParser "Class python3_ " [Ident "Class", Ident "python3_"]
    testParser "Haskell i_s thE   BeSt LanGUage     _ " [ Ident "Haskell", Ident "i_s", Ident "thE", Ident "BeSt"
                                                        , Ident "LanGUage", Ident "_"
                                                        ]

    putStrLn ""

    putStrLn "Test: Parsing keywords."

    testParser "for if   in True  " [KeyWord "for", KeyWord "if", KeyWord "in", KeyWord "True"]
    testParser "  else if  False in  " [KeyWord "else", KeyWord "if", KeyWord "False", KeyWord "in"]

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
                                                     , Number 8181810000
                                                     ]
