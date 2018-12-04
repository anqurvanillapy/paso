module Paso.Main where

import           System.IO

hRunPaso :: Handle -> IO ()
hRunPaso h = do
  src <- hGetContents h
  print src
