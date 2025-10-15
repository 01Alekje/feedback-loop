module Main where

import System.Environment.Blank (getEnvDefault)

{- agda <- startAgda
res <- runExceptT (test agda)
print res -}
main :: IO ()
main = do
  apiKey <- getEnvDefault "OPENAI_API_KEY" "NONE"
  print apiKey