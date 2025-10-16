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

previousResponsesFile :: String
previousResponsesFile = "/tmp/prev_resp_file"

main :: IO ()
main = do
  agda <- startAgda
  res <- runExceptT (load agda)
  TIO.writeFile previousResponsesFile (TL.pack "")
  case res of
    Left err -> print err
    Right holes -> feedbackLoop holes agda 0

feedbackLoop :: LoadData -> AgdaProc -> Int -> IO ()
feedbackLoop holes agda counter =
  if counter == 10
    then return ()
    else do
      writeContext holes
      response <- queryNano
      case parse (BL.pack response) of
        Left err -> print err
        Right command -> case command of
          Give hole expr -> runExceptT (giveAndReload holes agda hole expr) >>= handleAgdaResponse response
          Auto hole -> runExceptT (autoAndReload holes agda hole) >>= handleAgdaResponse response
          AddBinders binders name -> runExceptT (addBinders agda name binders) >>= handleAgdaResponse response
          CaseSplit hole binder -> runExceptT (caseSplit holes agda hole binder) >>= handleAgdaResponse response
  where
    writeContext :: LoadData -> IO ()
    writeContext holes' = TIO.writeFile contextFile (TL.pack (prettyHoles holes'))

    queryNano :: IO String
    queryNano = readProcess "python3" ["ApiService.py", codeFile, contextFile, previousResponsesFile] ""

    handleErr :: String -> ResponseError -> IO ()
    handleErr resp err = case err of
      HoleNotFound hole -> do
        TIO.appendFile previousResponsesFile $ TL.pack $ resp ++ " - " ++ "Error: " ++ "Hole " ++ show hole ++ " not found in context." ++ " - " ++ "Request " ++ show counter ++ "\n"
        feedbackLoop holes agda (counter + 1)
      AgdaError msg -> do
        TIO.appendFile previousResponsesFile $ TL.pack $ resp ++ " - " ++ "Error: " ++ msg ++ " - " ++ "Request " ++ show counter ++ "\n"
        feedbackLoop holes agda (counter + 1)
      UnknownResponse -> print "Got an unknown response from agda"

    handleSuccess :: String -> LoadData -> IO ()
    handleSuccess resp holes' = do
      TIO.appendFile previousResponsesFile $ TL.pack $ resp ++ " - " ++ "Success" ++ " - " ++ "Request " ++ show counter ++ "\n"
      feedbackLoop holes' agda (counter + 1)

    handleAgdaResponse :: String -> Either ResponseError LoadData -> IO ()
    handleAgdaResponse resp res = case res of
      Left err -> handleErr resp err
      Right holes' -> handleSuccess resp holes'
