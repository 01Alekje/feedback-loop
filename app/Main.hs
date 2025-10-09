module Main where

import AgdaProc (startAgda)
import Command

main :: IO ()
main = do
  agda <- startAgda
  test agda
