{-# LANGUAGE LambdaCase #-}

module Main where

import           System.Environment
import           System.Exit
import           System.IO

main :: IO ()
main =
  getArgs >>= \case
    [srcFile] -> do
      h <- openFile srcFile ReadMode
      src <- hGetContents h
      print src
      hClose h
    _ -> do
      exe <- getProgName
      hPutStrLn stderr $ "Usage: " ++ exe ++ " [FILE]"
      exitFailure
