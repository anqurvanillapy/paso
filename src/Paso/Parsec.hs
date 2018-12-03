{-# LANGUAGE LambdaCase #-}

module Paso.Parsec where

newtype Parser a = Parser
  { parse :: String -> [(a, String)]
  }

runParser :: Parser a -> String -> a
runParser p s =
  case parse p s of
    [(x, [])] -> x
    [(_, xs)] -> error "entire stream not cunsumed"
    _         -> error "parse error"

item :: Parser Char
item = Parser $ \case
  []      -> []
  (x:xs)  -> [(x, xs)]

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s ->
  concatMap (\(a, s') -> parse (f a) s') (parse p s)

unit :: a -> Parser a
unit a = Parser $ \s -> [(a, s)]
