module Paso.Concrete where

import           Control.Applicative
import           Data.Char

import           Paso.Parsec

reservedWords :: [String]
reservedWords =
  [ "context"
  , "postulate"
  , "Type"
  , "data"
  , "codata"
  ]

identifier :: Parser String
identifier = do
  s <- many alphaChar
  cs <- many . satisfy $ \c -> isAlphaNum c || c == '\''
  return $ checked (s ++ cs)
  where
    checked x = if x `elem` reservedWords
                then error $ "invalid identifier `" ++ x ++ "'"
                else x

type Name = String

data Expr
  = App Expr Expr
  | Term Name
