{-# LANGUAGE LambdaCase #-}

module Paso.Parsec
  ( Parser (..)
  , runParser
  , satisfy
  , oneOf
  , chainl
  , chainl1
  , manyTill
  , char
  , anyChar
  , alphaChar
  , alphaNumChar
  , string
  , token
  , reserved
  , spaces
  , digit
  , natural
  , number
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Char

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

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> [(f a, b) | (a, b) <- p s]

instance Applicative Parser where
  pure = return
  (Parser p1) <*> (Parser p2) = Parser $ \s ->
    [(f a, s2) | (f, s1) <- p1 s, (a, s2) <- p2 s1]

instance Monad Parser where
  return = unit
  (>>=)  = bind

instance MonadPlus Parser where
  mzero = failure
  mplus = combine

instance Alternative Parser where
  empty = mzero
  (<|>) = option

combine :: Parser a -> Parser a -> Parser a
combine p q = Parser $ \s -> parse p s ++ parse q s

failure :: Parser a
failure = Parser $ \s -> []

option :: Parser a -> Parser a -> Parser a
option p q = Parser $ \s ->
  case parse p s of
    [] -> parse q s
    xs -> xs

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = item `bind` \c ->
  if f c
  then unit c
  else failure

{- Combinators -}

oneOf :: String -> Parser Char
oneOf s = satisfy (flip elem s)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do
  a <- p
  rest a
  where rest a = (do f <- op
                     b <- p
                     rest (f a b))
                 <|> return a

manyTill :: Parser a -> Parser b -> Parser [a]
manyTill p q = scan where
  scan =     do { _ <- q; return [] }
         <|> do { x <- p; xs <- scan; return (x:xs) }

char :: Char -> Parser Char
char c = satisfy (c ==)

anyChar :: Parser Char
anyChar = satisfy (const True)

alphaChar :: Parser Char
alphaChar = satisfy isAlpha

alphaNumChar :: Parser Char
alphaNumChar = satisfy isAlphaNum

string :: String -> Parser String
string []     = return []
string (c:cs) = do
  char c
  string cs
  return (c:cs)

token :: Parser a -> Parser a
token p = do
  a <- p
  spaces
  return a

reserved :: String -> Parser String
reserved s = token (string s)

spaces :: Parser String
spaces = many $ oneOf " \n\r"

digit :: Parser Char
digit = satisfy isDigit

natural :: Parser Integer
natural = read <$> some (satisfy isDigit)

number :: Parser Int
number = do
  s <- string "-" <|> return []
  cs <- some digit
  return $ read (s ++ cs)

parens :: Parser a -> Parser a
parens p = do
  reserved "("
  n <- p
  reserved ")"
  return n
