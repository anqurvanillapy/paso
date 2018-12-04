{-# LANGUAGE LambdaCase #-}

module Main where

import           System.Environment
import           System.Exit
import           System.IO

import           Paso.Main

main :: IO ()
main =
  getArgs >>= \case
    [srcFile] -> do
      withFile srcFile ReadMode $ \h -> do
        hRunPaso h
    _ -> do
      exe <- getProgName
      hPutStrLn stderr $ "Usage: " ++ exe ++ " [FILE]"
      exitFailure
