{-# LANGUAGE FlexibleInstances #-}      

module Expr where

import Control.Applicative
import Data.Char
import Text.Printf

data AST = Num Int
         | BinOp Char AST AST

anyChar = foldl1 (<|>) . map char

digit = digitToInt <$> anyChar "0123456789"
ops = anyChar "-+"
lbr = char '('
rbr = char ')'

runParser = parse (expr id)

expr k =
  do
    n  <- num
    op <- ops
    e <-
      if op == '-'
      then expr (\y -> BinOp op (k n) y)
      else BinOp op (k n) <$> expr id
    return e
  <|>
  do
    n <- num
    return $ k n
  where
    num = Num <$> digit <|> do
    _ <- lbr
    e <- expr id
    _ <- rbr
    return e


-- Parser combinators 

-- Input abstraction
type Input = String

-- Result is polymorphic in the ... result
data Result r = Success r
              | Error String

instance Show r => Show (Result (r, Input)) where 
  show (Success (r, xs)) = printf "%s\n%s" (show r) xs
  show (Error err) = printf "Parsing error: %s" err

-- The result of parsing is some payload r and the suffix which wasn't parsed
newtype Parser r = Parser { parse :: Input -> Result (r, Input) }

instance Applicative Parser where
  pure = return

  p <*> q = Parser $ \inp ->
    case parse p inp of
      Success (f, inp') ->
        case parse q inp' of
          Success (a, inp'') -> Success (f a, inp'')
          Error err -> Error err
      Error err -> Error err


instance Alternative Parser where
  -- Choice combinator: checks if the input can be parsed with either the first, or the second parser
  -- Left biased: make sure, that the first parser consumes more input
  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = Parser $ \inp ->
    case parse p inp of
      Error _ -> parse q inp
      result  -> result

  -- Always fails
  empty = Parser $ const $ Error ""

instance Monad Parser where
  -- Succeedes without consuming any input, returning a value
  -- return :: a -> Parser a
  return r = Parser $ \inp -> Success (r, inp)

  --  Sequential combinator: if the first parser successfully parses some prefix, the second is run on the suffix
  -- The second parser is supposed to use the result of the first parser
  -- (>>=) :: Parser a -> (a -> Parser b ) -> Parser b
  p >>= q = Parser $ \inp ->
    case parse p inp of
      Success (r, inp') -> parse (q r) inp'
      Error err -> Error err

instance Functor Parser where
  -- Applies the function to the result of the parser
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f parser = Parser $ \inp ->
    case parse parser inp of
      Success (r, inp') -> Success (f r, inp')
      Error err -> Error err

item :: Parser Char
item = Parser $ \inp ->
  case inp of
    [] -> Error "Empty string"
    (c:cs) -> Success (c, cs)

-- Checks if the parser result satisfies the predicate
satisfy :: (a -> Bool) -> Parser a -> Parser a
satisfy pred parser = Parser $ \inp ->
  case parse parser inp of
    Success (r, inp') | pred r ->  Success (r, inp')
    Success (_, inp') -> Error (printf "Predicate is not satisfied on: %s" inp')
    Error err -> Error err

char :: Char -> Parser Char
char c = satisfy (== c) item

-- Pretty printing for trees

instance Show AST where
  show = show' 0
    where
      show' n t =
        (if n > 0 then printf "%s|_%s" (concat (replicate (n - 1) "| ")) else id)
        (case t of
                  BinOp op l r -> printf "%s\n%s\n%s" [op] (show' (ident n) l) (show' (ident n) r)
                  Num x -> show x)
      ident = (+1)
