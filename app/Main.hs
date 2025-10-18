module Main where

import AgdaProc (AgdaProc, startAgda)
import Command
import Control.Monad.Except (runExceptT)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as TL
import qualified Data.Text.IO as TIO
import Reply
import System.Process (readProcess)

contextFile :: String
contextFile = "/tmp/context_file"

codeFile :: String
codeFile = "Example.agda"

tmpFile :: String
tmpFile = "/tmp/Example.agda"

previousResponsesFile :: String
previousResponsesFile = "/tmp/prev_resp_file"

main :: IO ()
main = do
  agda <- startAgda
  codeContents <- TIO.readFile codeFile
  TIO.writeFile tmpFile (TL.pack "")
  TIO.writeFile tmpFile codeContents

  res <- runExceptT (load agda)

  TIO.writeFile previousResponsesFile (TL.pack "")
  case res of
    Left err -> print err
    Right holes -> feedbackLoop holes agda 0

feedbackLoop :: LoadData -> AgdaProc -> Int -> IO ()
feedbackLoop holes agda counter = do
      writeContext holes
      putStrLn "Querying gpt-5..."
      putStrLn "Waiting for response..."
      response <- queryNano
      putStrLn $ "Got response: " ++ response
      case parse (BL.pack response) of
        Left err -> putStrLn err
        Right command -> case command of
          Give hole expr -> runExceptT (give holes agda hole expr) >>= handleAgdaResponse response
          Auto hole -> runExceptT (auto holes agda hole) >>= handleAgdaResponse response
          AddBinders binders name -> runExceptT (addBinders agda name binders) >>= handleAgdaResponse response
          CaseSplit hole binder -> runExceptT (caseSplit holes agda hole binder) >>= handleAgdaResponse response
          Reset -> handleReset response
  where
    writeContext :: LoadData -> IO ()
    writeContext holes' = TIO.writeFile contextFile (TL.pack (prettyHoles holes'))

    queryNano :: IO String
    queryNano = do
      readProcess "python3" ["ApiService.py", tmpFile, contextFile, previousResponsesFile] ""

    handleErr :: String -> ResponseError -> IO ()
    handleErr resp err = case err of
      HoleNotFound hole -> do
        putStrLn "Trying to complete"
        TIO.appendFile previousResponsesFile $ TL.pack $ resp ++ " - " ++ "Error: " ++ "Hole " ++ show hole ++ " not found in context." ++ " - " ++ "Request " ++ show counter ++ "\n"
        feedbackLoop holes agda (counter + 1)
      AgdaError msg -> do
        TIO.appendFile previousResponsesFile $ TL.pack $ resp ++ " - " ++ "Error: " ++ msg ++ " - " ++ "Request " ++ show counter ++ "\n"
        feedbackLoop holes agda (counter + 1)
      UnknownResponse -> putStrLn "Got an unknown response from agda"

    handleSuccess :: String -> LoadData -> IO ()
    handleSuccess resp holes' = do
      TIO.appendFile previousResponsesFile $ TL.pack $ resp ++ " - " ++ "Success" ++ " - " ++ "Request " ++ show counter ++ "\n"
      feedbackLoop holes' agda (counter + 1)

    handleAgdaResponse :: String -> Either ResponseError LoadData -> IO ()
    handleAgdaResponse resp res = case res of
      Left err -> handleErr resp err
      Right holes' -> case holes' of
        [] -> do
          putStrLn "All Done!"
          finFile <- TIO.readFile tmpFile
          TIO.writeFile codeFile finFile
        _ -> handleSuccess resp holes'

    handleReset :: String -> IO ()
    handleReset resp = do
      codeContents <- TIO.readFile codeFile
      TIO.writeFile tmpFile (TL.pack "")
      TIO.writeFile tmpFile codeContents
      resHoles <- runExceptT (load agda)
      case resHoles of
        Left err -> print err
        Right resetHoles -> handleSuccess resp resetHoles