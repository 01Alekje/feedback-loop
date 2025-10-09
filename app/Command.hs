module Command (load, test) where

import AgdaProc (AgdaProc (..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.String as BS
import qualified Data.Text as TL
import DisplayInfo
import System.IO (hFlush, hGetLine, hIsEOF, hPutStrLn)
import Prelude hiding (exp, id)

type LoadData = [(Int, Hole)]

data Hole = Hole
  { holeType :: String,
    holeRange :: Range,
    context :: [(String, String, Bool)] -- (var, type)
  }
  deriving (Show)

autoString :: Int -> String
autoString hole = "IOTCM \"Example.agda\" None Indirect (Cmd_autoOne AsIs " ++ show hole ++ " noRange \"\")"

loadString :: String
loadString = "IOTCM \"Example.agda\" None Indirect (Cmd_load \"Example.agda\" [])"

contextString :: Int -> String
contextString hole = "IOTCM \"Example.agda\" None Indirect (Cmd_goal_type_context Simplified " ++ show hole ++ " noRange \"\")"

giveString :: Int -> String -> String
giveString hole exp = "IOTCM \"Example.agda\" None Indirect (Cmd_give WithoutForce " ++ show hole ++ " noRange " ++ "\"" ++ exp ++ "\"" ++ ")"

sendCommand :: AgdaProc -> String -> IO ()
sendCommand agda str = do
  hPutStrLn (agdaIn agda) str
  hFlush (agdaIn agda)

load :: AgdaProc -> IO LoadData
load agda = do
  sendCommand agda loadString
  resp <- getResponse agda
  case resp of
    ResponseDisplay (DisplayInfo kind info) -> case info of
      AllGoalsWarnings _ _ vs _ ->
        let res = map (\(VisibleGoal (ConstraintObj id [range]) _ _) -> (range, id)) vs
         in do
              holes <- mapM (uncurry getHole) res
              case sequence holes of
                Just xs -> pure xs
                Nothing -> pure []
      _ -> pure []
    _ -> pure []
  where
    getHole :: Range -> Int -> IO (Maybe (Int, Hole))
    getHole range id = do
      sendCommand agda (contextString id)
      resp <- getResponse agda
      case resp of
        ResponseDisplay (DisplayInfo _ info') -> case info' of
          GoalSpecific (GoalInfo _ entries _ _ _ goalType _) _ ->
            pure $ Just $ (id, Hole (TL.unpack goalType) range (map (\(GoalEntry ty inScope name _) -> (TL.unpack name, TL.unpack ty, inScope)) entries))
          _ -> pure Nothing
        _ -> pure Nothing

-- VisibleGoal (ConstraintObj id _) _ _
test :: AgdaProc -> IO ()
test agda = do
  holes <- load agda
  let hole = lookup 0 holes
   in case hole of
        Nothing -> print "no such hole"
        Just something -> replaceHoleInFile "test" something

replaceHoleInFile :: String -> Hole -> IO ()
replaceHoleInFile exp hole = do
  contents <- readFile "Example.agda"
  writeFile "Example.agda" (replaceHole (positions hole) contents exp)
  where
    replaceHole :: (Int, Int) -> String -> String -> String
    replaceHole (start, end) xs exp' =
      let (left, xs') = splitAt start xs
       in let (_, right) = splitAt (end - start) xs'
           in left ++ exp' ++ right

    positions :: Hole -> (Int, Int)
    positions hole' =
      let range = holeRange hole'
       in (posPosition (startRange range), posPosition (endRange range))

getResponse :: AgdaProc -> IO Response
getResponse agda = do
  agdaLine <- BS.hGetLine (agdaOut agda)
  let agdaLine' =
        if BS.fromString "JSON> " `BS.isPrefixOf` agdaLine
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
