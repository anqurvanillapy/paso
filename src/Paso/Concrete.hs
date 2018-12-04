module Paso.Concrete where

import           Paso.Parsec
import           Paso.Position

data Expr
  = Lit String
  deriving Show

type RawName = String

type TypeSig = Expr

data Induction
  = Data
  | Codata
  deriving Show

data Decl
  = Context   SrcInfo RawName
  | DataType  SrcInfo Induction TypeSig Expr
  | Postulate SrcInfo [Decl]
  deriving Show

data Top
  = Decl SrcInfo Top
  | EOF SrcInfo
