module Command (load, test) where

import AgdaProc (AgdaProc (..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List (isPrefixOf)
import qualified Data.String as BS
import qualified Data.Text as TL
import qualified Data.Text.IO as TIO
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

data ResponseError
  = HoleNotFound Int
  | AgdaError String
  | UnknownResponse
  deriving (Show)

qoute :: String -> String
qoute str = "\"" ++ str ++ "\""

exampleFile :: String
exampleFile = "Example.agda"

autoString :: Int -> String
autoString hole = "IOTCM " ++ qoute exampleFile ++ " None Indirect (Cmd_autoOne AsIs " ++ show hole ++ " noRange " ++ qoute [] ++ ")"

loadString :: String
loadString = "IOTCM " ++ qoute exampleFile ++ " None Indirect (Cmd_load " ++ qoute exampleFile ++ " [])"

contextString :: Int -> String
contextString hole = "IOTCM " ++ qoute exampleFile ++ " None Indirect (Cmd_goal_type_context Simplified " ++ show hole ++ " noRange " ++ qoute [] ++ ")"

giveString :: Int -> String -> String
giveString hole exp = "IOTCM " ++ qoute exampleFile ++ " None Indirect (Cmd_give WithoutForce " ++ show hole ++ " noRange " ++ qoute exp ++ ")"

caseString :: Int -> String -> String
caseString hole binder = "IOTCM " ++ qoute exampleFile ++ " None Indirect (Cmd_make_case " ++ show hole ++ " noRange " ++ qoute binder ++ ")"

sendCommand :: AgdaProc -> String -> IO ()
sendCommand agda str = do
  hPutStrLn (agdaIn agda) str
  hFlush (agdaIn agda)

load :: AgdaProc -> IO LoadData
load agda = do
  sendCommand agda loadString
  resp <- getResponse agda
  case resp of
    ResponseDisplay (DisplayInfo _ info') -> case info' of
      AllGoalsWarnings _ _ vs _ -> do
        holes <- mapM (uncurry getHole . getRangeAndId) vs
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
            pure $ Just (id, Hole (TL.unpack goalType) range (map (\(GoalEntry ty inScope name _) -> (TL.unpack name, TL.unpack ty, inScope)) entries))
          _ -> pure Nothing
        _ -> pure Nothing

    getRangeAndId :: VisibleGoal -> (Range, Int)
    getRangeAndId (VisibleGoal (ConstraintObj id rs) _ _) = case rs of
      [range] -> (range, id)
      _ -> undefined

test :: AgdaProc -> IO ()
test agda = do
  _ <- caseSplit agda 4 "pq"
  holes <- load agda
  holes' <- giveAndReload holes agda 4 "inr x"
  case holes' of
    Left _ -> return ()
    Right holes'' -> do
      _ <- giveAndReload holes'' agda 4 "inl x"
      return ()

caseSplit :: AgdaProc -> Int -> String -> IO (Either ResponseError ())
caseSplit agda id binder = do
  holes <- load agda
  case lookup id holes of
    Nothing -> return $ Left $ HoleNotFound id
    Just hole -> do
      sendCommand agda (caseString id binder)
      resp <- getResponse agda
      case resp of
        ResponseDisplay (DisplayInfo _ info') -> case info' of
          ErrorInfo (ErrorDetails msg) _ -> return $ Left $ AgdaError $ TL.unpack msg
          _ -> return $ Left UnknownResponse
        ResponseMakeCase (MakeCase clauses' _ _) ->
          let clauses = map TL.unpack clauses'
           in do
                contents <- lines . TL.unpack <$> TIO.readFile exampleFile
                TIO.writeFile exampleFile $ TL.pack $ unlines $ addCasesLines hole clauses contents
                sendCommand agda loadString
                resp' <- getResponse agda
                case resp' of
                  ResponseDisplay (DisplayInfo _ info'') -> case info'' of
                    ErrorInfo (ErrorDetails msg) _ -> do
                      TIO.writeFile exampleFile $ TL.pack $ unlines contents
                      return $ Left $ AgdaError $ TL.unpack msg
                    AllGoalsWarnings {} -> return $ Right ()
                    _ -> return $ Left UnknownResponse
                  _ -> return $ Left UnknownResponse
        _ -> return $ Left UnknownResponse

addCasesLines :: Hole -> [String] -> [String] -> [String]
addCasesLines hole cases lines' =
  let (start, end) = splitAt (splitPos - 1) lines' in start ++ cases ++ tail end
  where
    splitPos :: Int
    splitPos = linePosition $ startRange (holeRange hole)

addBinders :: AgdaProc -> String -> [String] -> IO (Either ResponseError ())
addBinders agda name binders = do
  contents <- lines . TL.unpack <$> TIO.readFile exampleFile
  TIO.writeFile exampleFile $ TL.pack $ unlines $ addBindersLines name binders contents
  sendCommand agda loadString
  resp <- getResponse agda
  case resp of
    ResponseDisplay (DisplayInfo _ info') -> case info' of
      ErrorInfo (ErrorDetails msg) _ -> do
        TIO.writeFile exampleFile $ TL.pack $ unlines contents
        return $ Left $ AgdaError $ TL.unpack msg
      AllGoalsWarnings {} -> return $ Right ()
      _ -> return $ Left UnknownResponse
    _ -> return $ Left UnknownResponse

addBindersLines :: String -> [String] -> [String] -> [String]
addBindersLines name binders = map addBindersLine
  where
    addBindersLine :: String -> String
    addBindersLine line =
      let wwl = lstrip line
       in let cond = isPrefixOf (name ++ "\t") wwl || isPrefixOf (name ++ " ") wwl
           in if not cond
                then line
                else case getIndex '=' line of
                  Nothing -> line
                  Just index ->
                    let (start, end) = splitAt index line
                     in start ++ unwords binders ++ " " ++ end
      where
        lstrip :: String -> String
        lstrip ('\t' : xs) = lstrip xs
        lstrip (' ' : xs) = lstrip xs
        lstrip xs = xs

        getIndex :: Char -> String -> Maybe Int
        getIndex e xs = go e (zip xs [0 ..])
          where
            go :: Char -> [(Char, Int)] -> Maybe Int
            go _ [] = Nothing
            go e' ((c, pos) : ys) = if e' == c then Just pos else go e' ys

giveAndReload :: LoadData -> AgdaProc -> Int -> String -> IO (Either ResponseError LoadData)
giveAndReload holes agda id exp = case lookup id holes of
  Nothing -> return $ Left $ HoleNotFound id
  Just hole -> do
    sendCommand agda (giveString id exp)
    resp <- getResponse agda
    case resp of
      ResponseGive (GiveResult (GiveResultContent str) _ _) -> do
        _ <- getResponse agda
        replaceHoleInFile (TL.unpack str) hole
        holes' <- load agda
        return $ Right holes'
      ResponseDisplay (DisplayInfo _ info') -> case info' of
        ErrorInfo (ErrorDetails msg) _ -> return $ Left $ AgdaError (TL.unpack msg)
        _ -> return $ Left UnknownResponse
      _ -> return $ Left UnknownResponse

autoAndReload :: LoadData -> AgdaProc -> Int -> IO (Either ResponseError LoadData)
autoAndReload holes agda id = case lookup id holes of
  Nothing -> return $ Left $ HoleNotFound id
  Just hole -> do
    sendCommand agda (autoString id)
    resp <- getResponse agda
    case resp of
      ResponseGive (GiveResult (GiveResultContent str) _ _) -> do
        replaceHoleInFile (TL.unpack str) hole
        holes' <- load agda
        return $ Right holes'
      ResponseDisplay (DisplayInfo _ info') -> case info' of
        AutoFailed msg _ -> return $ Left $ AgdaError (TL.unpack msg)
        _ -> return $ Left UnknownResponse
      _ -> return $ Left UnknownResponse

replaceHoleInFile :: String -> Hole -> IO ()
replaceHoleInFile exp hole = do
  contents <- TL.unpack <$> TIO.readFile exampleFile
  TIO.writeFile exampleFile (TL.pack $ replaceHole (positions hole) contents exp)
  where
    replaceHole :: (Int, Int) -> String -> String -> String
    replaceHole (start, end) xs exp' =
      let (left, xs') = splitAt (start - 1) xs
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
