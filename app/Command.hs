{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Command (load, LoadData, prettyHoles, autoAndReload, giveAndReload, addBinders, caseSplit, ResponseError (..)) where

import AgdaProc (AgdaProc (..))
import Control.Monad.Except
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List (intercalate, isPrefixOf)
import qualified Data.String as BS
import qualified Data.Text as TL
import qualified Data.Text.IO as TIO
import Response
import System.IO (hFlush, hPutStrLn)
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

-- | Convenient alias
type AgdaM a = ExceptT ResponseError IO a

sendCommand :: AgdaProc -> String -> AgdaM ()
sendCommand agda str = liftIO $ do
  hPutStrLn (agdaIn agda) str
  hFlush (agdaIn agda)

load :: AgdaProc -> AgdaM LoadData
load agda = do
  sendCommand agda loadString
  resp <- liftIO $ getResponse agda
  case resp of
    ResponseDisplay (DisplayInfo _ info') -> case info' of
      ErrorInfo (ErrorDetails msg) _ -> throwError $ AgdaError $ TL.unpack msg
      AllGoalsWarnings _ _ vs _ -> do
        holes <- liftIO $ mapM (uncurry getHole . getRangeAndId) vs
        pure $ foldr (\x acc -> maybe acc (: acc) x) [] holes
      _ -> throwError UnknownResponse
    _ -> throwError UnknownResponse
  where
    getHole :: Range -> Int -> IO (Maybe (Int, Hole))
    getHole range id = do
      sendCommand' agda (contextString id)
      resp <- getResponse agda
      case resp of
        ResponseDisplay (DisplayInfo _ info') -> case info' of
          GoalSpecific (GoalInfo _ entries _ _ _ goalType _) _ ->
            pure $
              Just
                ( id,
                  Hole
                    (TL.unpack goalType)
                    range
                    (map (\(GoalEntry ty inScope name _) -> (TL.unpack name, TL.unpack ty, inScope)) entries)
                )
          _ -> pure Nothing
        _ -> pure Nothing

    sendCommand' a s = do
      hPutStrLn (agdaIn a) s
      hFlush (agdaIn a)

    getRangeAndId :: VisibleGoal -> (Range, Int)
    getRangeAndId (VisibleGoal (ConstraintObj id rs) _ _) = case rs of
      [range] -> (range, id)
      _ -> undefined

caseSplit :: LoadData -> AgdaProc -> Int -> String -> AgdaM LoadData
caseSplit holes agda id binder = do
  case lookup id holes of
    Nothing -> throwError $ HoleNotFound id
    Just hole -> do
      sendCommand agda (caseString id binder)
      resp <- liftIO $ getResponse agda
      case resp of
        ResponseDisplay (DisplayInfo _ info') -> case info' of
          ErrorInfo (ErrorDetails msg) _ -> throwError $ AgdaError $ TL.unpack msg
          _ -> throwError UnknownResponse
        ResponseMakeCase (MakeCase clauses' _ _) -> do
          let clauses = map TL.unpack clauses'
          contents <- liftIO $ lines . TL.unpack <$> TIO.readFile exampleFile
          liftIO $ TIO.writeFile exampleFile $ TL.pack $ unlines $ addCasesLines hole clauses contents
          catchError
            (load agda)
            ( \e -> do
                liftIO $ TIO.writeFile exampleFile $ TL.pack $ unlines contents
                throwError e
            )
        _ -> throwError UnknownResponse

addCasesLines :: Hole -> [String] -> [String] -> [String]
addCasesLines hole cases lines' =
  let (start, end) = splitAt (splitPos - 1) lines' in start ++ cases ++ tail end
  where
    splitPos = linePosition $ startRange (holeRange hole)

addBinders :: AgdaProc -> String -> [String] -> AgdaM LoadData
addBinders agda name binders = do
  contents <- liftIO $ lines . TL.unpack <$> TIO.readFile exampleFile
  liftIO $ TIO.writeFile exampleFile $ TL.pack $ unlines $ addBindersLines name binders contents
  result <- liftIO $ runExceptT (load agda)
  case result of
    Left err -> do
      liftIO $ TIO.writeFile exampleFile $ TL.pack $ unlines contents
      throwError err
    Right val -> return val

addBindersLines :: String -> [String] -> [String] -> [String]
addBindersLines name binders = map addBindersLine
  where
    addBindersLine line =
      let wwl = lstrip line
          cond = isPrefixOf (name ++ "\t") wwl || isPrefixOf (name ++ " ") wwl
       in if not cond
            then line
            else case getIndex '=' line of
              Nothing -> line
              Just index ->
                let (start, end) = splitAt index line
                 in start ++ unwords binders ++ " " ++ end
    lstrip ('\t' : xs) = lstrip xs
    lstrip (' ' : xs) = lstrip xs
    lstrip xs = xs
    getIndex e xs = lookupIndex e (zip xs [0 ..])
    lookupIndex _ [] = Nothing
    lookupIndex e' ((c, pos) : ys) = if e' == c then Just pos else lookupIndex e' ys

giveAndReload :: LoadData -> AgdaProc -> Int -> String -> AgdaM LoadData
giveAndReload holes agda id exp = case lookup id holes of
  Nothing -> throwError $ HoleNotFound id
  Just hole -> do
    sendCommand agda (giveString id exp)
    resp <- liftIO $ getResponse agda
    case resp of
      ResponseGive (GiveResult (GiveResultContent str) _ _) -> do
        _ <- liftIO $ getResponse agda
        liftIO $ replaceHoleInFile (TL.unpack str) hole
        load agda
      ResponseDisplay (DisplayInfo _ info') -> case info' of
        ErrorInfo (ErrorDetails msg) _ -> throwError $ AgdaError (TL.unpack msg)
        _ -> throwError UnknownResponse
      _ -> throwError UnknownResponse

autoAndReload :: LoadData -> AgdaProc -> Int -> AgdaM LoadData
autoAndReload holes agda id = case lookup id holes of
  Nothing -> throwError $ HoleNotFound id
  Just hole -> do
    sendCommand agda (autoString id)
    resp <- liftIO $ getResponse agda
    case resp of
      ResponseGive (GiveResult (GiveResultContent str) _ _) -> do
        liftIO $ replaceHoleInFile (TL.unpack str) hole
        load agda
      ResponseDisplay (DisplayInfo _ info') -> case info' of
        AutoFailed msg _ -> throwError $ AgdaError (TL.unpack msg)
        _ -> throwError UnknownResponse
      _ -> throwError UnknownResponse

replaceHoleInFile :: String -> Hole -> IO ()
replaceHoleInFile exp hole = do
  contents <- TL.unpack <$> TIO.readFile exampleFile
  TIO.writeFile exampleFile (TL.pack $ replaceHole (positions hole) contents exp)
  where
    replaceHole (start, end) xs exp' =
      let (left, xs') = splitAt (start - 1) xs
          (_, right) = splitAt (end - start) xs'
       in left ++ exp' ++ right

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
    Left _ -> getResponse agda
    Right disp -> pure disp

qoute :: String -> String
qoute str = "\"" ++ str ++ "\""

exampleFile :: String
exampleFile = "/tmp/tempFile.agda"

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

prettyHoles :: LoadData -> String
prettyHoles holes = intercalate "\n" (map prettyHole holes)
  where
    prettyHole :: (Int, Hole) -> String
    prettyHole (id, Hole {holeType = ty, holeRange = range, context = ctx}) =
      "Hole " ++ show id ++ "\n\t" ++ "Goal Type: " ++ ty ++ "\n\t" ++ prettyRange range ++ "\n\t" ++ "Context:\n" ++ concatMap (("\t" ++) . prettyVar) ctx

    prettyVar :: (String, String, Bool) -> String
    prettyVar (name, ty, inScope) = name ++ " : " ++ ty ++ if inScope then " (Is in Scope)" else " (Is not in scope)" ++ "\n"

    prettyRange :: Range -> String
    prettyRange (Range (Position col1 line1 pos1) (Position col2 line2 pos2)) =
      "start = (col = "
        ++ show col1
        ++ ", line = "
        ++ show line1
        ++ ", pos = "
        ++ show pos1
        ++ ")\n\t"
        ++ "end = (col = "
        ++ show col2
        ++ ", line = "
        ++ show line2
        ++ ", pos = "
        ++ show pos2
        ++ ")"
