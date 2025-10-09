module AgdaProc (AgdaProc(..), startAgda) where

import System.IO (Handle)
import System.Process (StdStream(..), std_in, std_out, std_err, ProcessHandle, createProcess, proc)

data AgdaProc = AgdaProc
  { 
    agdaIn  :: Handle
  , agdaOut :: Handle
  , agdaErr :: Handle
  , agdaPH  :: ProcessHandle
  }

startAgda :: IO AgdaProc
startAgda = do
  (Just hin, Just hout, Just herr, ph) <-
    createProcess (proc "agda" ["--interaction-json"])
      { std_in  = CreatePipe
      , std_out = CreatePipe
      , std_err = CreatePipe
      }
  putStrLn "Started Agda process in JSON interaction mode."
  pure $ AgdaProc hin hout herr ph