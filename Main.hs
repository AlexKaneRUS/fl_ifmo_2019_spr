module Main where

import           Automaton
import           Combinators
import           Control.Applicative (many, some, (<|>))
import           Expression
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
