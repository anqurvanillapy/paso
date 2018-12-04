module Paso.Lexer where

import           Paso.Parsec
import           Paso.Position

data Token
  = TkComment String
  | TkParenL
  | TkParenR
  | TkBraceL
  | TkBraceR
  | TkComma
  | TkDot
  | TkColon
  | TkAssign
  | TkCoprod
  | TkArrowR
  | TkCtx
  | TkData
  | TkCodata
  | TkForall
  | TkExists
  | TkPostulate
  | TkType
  | TkUserOp String
  | TkIdent String
  | TkEOF

type Tokens = [Token]

-- tkComment :: Parser String
-- tkComment = do
--   string "(*"
--   manyTill anyChar . try $ string "*)"

tkParenL :: Parser String
tkParenL = reserved "("

tkParenR :: Parser String
tkParenR = reserved ")"
