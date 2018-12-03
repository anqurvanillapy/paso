module Paso.Concrete where

import           Paso.Parsec

data Expr
  = Lit String
  deriving Show

data SrcInfo = SrcInfo
  { srcFile :: FilePath
  , pos     :: Int
  , line    :: Int
  , col     :: Int
  }
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

type Top = [Decl]
