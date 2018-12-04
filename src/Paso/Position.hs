module Paso.Position
  ( SrcInfo
  ) where

data SrcInfo = SrcInfo
  { srcFile :: FilePath
  , pos     :: Int
  , line    :: Int
  , col     :: Int
  }
  deriving Show
