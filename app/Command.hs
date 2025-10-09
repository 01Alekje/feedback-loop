module Command (load, test) where

import AgdaProc (AgdaProc(..))
import DisplayInfo 
import qualified Data.ByteString as BS
import System.IO (hPutStrLn, hFlush, hGetLine, hIsEOF)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.String as BS
import Text.ParserCombinators.ReadP (get)

autoString :: String -> String
autoString hole = "IOTCM \"Example.agda\" None Indirect (Cmd_autoOne AsIs " ++ hole ++ " noRange \"\")"

loadString :: String
loadString = "IOTCM \"Example.agda\" None Indirect (Cmd_load \"Example.agda\" [])"

contextString :: String -> String
contextString hole = "IOTCM \"Example.agda\" None Indirect (Cmd_goal_type_context Simplified " ++ hole ++ " noRange \"\")"

giveString :: String -> String -> String
giveString hole exp = "IOTCM \"Example.agda\" None Indirect (Cmd_give WithoutForce " ++ hole ++ " noRange " ++ "\"" ++ exp ++ "\"" ++ ")"

sendCommand :: AgdaProc -> String -> IO ()
sendCommand agda str = do 
    hPutStrLn (agdaIn agda) str
    hFlush (agdaIn agda)    

load :: AgdaProc -> IO ()
load agda = do 
    sendCommand agda loadString
    resp <- getResponse agda
    case resp of 
        ResponseDisplay disp -> print disp
        _ -> putStrLn "Unexpected response"
    

test :: AgdaProc -> IO ()
test agda = do
    sendCommand agda loadString
    --sendCommand agda $ giveString "0" "pq smeku"
    --sendCommand agda $ autoString "2"
    getResponse agda
    getResponse agda >>= print
    --readUntilEof agda
    


getResponse :: AgdaProc -> IO Response
getResponse agda = do
    agdaLine <- BS.hGetLine (agdaOut agda)
    let agdaLine' = if BS.fromString "JSON> " `BS.isPrefixOf` agdaLine 
                    then BS.drop 6 agdaLine 
                    else agdaLine
    case parse (BSL.fromStrict agdaLine') of 
        Left _ -> do
            getResponse agda
        Right disp -> pure disp

    
readUntilEof :: AgdaProc -> IO ()
readUntilEof agda = do
    eof <- hIsEOF (agdaOut agda)
    if eof 
        then putStrLn "Reached EOF"
        else do
            line' <- hGetLine (agdaOut agda)
            putStrLn line'
            readUntilEof agda
