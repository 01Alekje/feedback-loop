module Main where

import AgdaProc (AgdaProc, startAgda)
import Command
import Control.Monad.Except (runExcept, runExceptT)
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
          Give hole expr -> do
            res <- runExceptT (giveAndReload holes agda hole expr)
            case res of
              Left err -> handleErr response err
              Right holes' -> do
                TIO.appendFile previousResponsesFile $ TL.pack $ response ++ " - " ++ "Success" ++ " - " ++ "Request " ++ show counter ++ "\n"
                feedbackLoop holes' agda (counter + 1)
          Auto hole -> do
            res <- runExceptT (autoAndReload holes agda hole)
            case res of
              Left err -> handleErr response err
              Right holes' -> do
                TIO.appendFile previousResponsesFile $ TL.pack $ response ++ " - " ++ "Success" ++ " - " ++ "Request " ++ show counter ++ "\n"
                feedbackLoop holes' agda (counter + 1)
          AddBinders binders name -> do
            res <- runExceptT (addBinders agda name binders)
            case res of
              Left err -> handleErr response err
              Right holes' -> do
                TIO.appendFile previousResponsesFile $ TL.pack $ response ++ " - " ++ "Success" ++ " - " ++ "Request " ++ show counter ++ "\n"
                feedbackLoop holes' agda (counter + 1)
          CaseSplit hole binder -> do
            res <- runExceptT (caseSplit holes agda hole binder)
            case res of
              Left err -> handleErr response err
              Right holes' -> do
                TIO.appendFile previousResponsesFile $ TL.pack $ response ++ " - " ++ "Success" ++ " - " ++ "Request " ++ show counter ++ "\n"
                feedbackLoop holes' agda (counter + 1)
  where
    writeContext :: LoadData -> IO ()
    writeContext holes = TIO.writeFile contextFile (TL.pack (prettyHoles holes))

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

{-
    Context
    PrevResp
    Example.agda

    appendFile
-}
