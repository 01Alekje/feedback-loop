{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Response
  ( Response (..),
    DisplayInfo (..),
    InfoContent (..),
    VisibleGoal (..),
    ConstraintObj (..),
    Range (..),
    Position (..),
    ErrorDetails (..),
    GoalInfo (..),
    GoalEntry (..),
    GiveResult (..),
    GiveResultContent (..),
    MakeCase (..),
    parse,
  )
where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import GHC.Generics

data Response
  = ResponseDisplay DisplayInfo
  | ResponseGive GiveResult
  | ResponseMakeCase MakeCase
  deriving (Show)

data DisplayInfo = DisplayInfo
  { kind :: Text,
    info :: InfoContent
  }
  deriving (Show, Generic)

data InfoContent
  = AllGoalsWarnings
      { errors :: [Value],
        invisibleGoals :: [Value],
        visibleGoals :: [VisibleGoal],
        warnings :: [Value]
      }
  | ErrorInfo
      { error :: ErrorDetails,
        warnings :: [Value]
      }
  | GoalSpecific
      { goalInfo :: GoalInfo,
        interactionPoint :: InteractionPoint
      }
  | AutoFailed
      { infoAutoFailed :: Text,
        autoKind :: Text
      }
  | OtherInfo Value
  deriving (Show)

data VisibleGoal = VisibleGoal
  { constraintObjVisible :: ConstraintObj,
    goalKindVisible :: Text,
    goalTypeVisible :: Text
  }
  deriving (Show, Generic)

data ConstraintObj = ConstraintObj
  { idConstraintObj :: Int,
    rangeConstraintObj :: [Range]
  }
  deriving (Show, Generic)

data Range = Range
  { startRange :: Position,
    endRange :: Position
  }
  deriving (Show, Generic)

data Position = Position
  { colPosition :: Int,
    linePosition :: Int,
    posPosition :: Int
  }
  deriving (Show, Generic)

data GoalInfo = GoalInfo
  { boundaryGoalInfo :: [Value],
    entriesGoalInfo :: [GoalEntry],
    kindGoalInfo :: Text,
    outputFormsGoalInfo :: [Value],
    rewriteGoalInfo :: Text,
    typeGoalInfo :: Text,
    typeAuxGoalInfo :: GoalTypeAux
  }
  deriving (Show, Generic)

data GoalEntry = GoalEntry
  { bindingGoalEntry :: Text,
    inScopeGoalEntry :: Bool,
    originalNameGoalEntry :: Text,
    reifiedNameGoalEntry :: Text
  }
  deriving (Show, Generic)

newtype GoalTypeAux = GoalTypeAux
  { kindGoalTypeAux :: Text
  }
  deriving (Show, Generic)

data InteractionPoint = InteractionPoint
  { idInteractionPoint :: Int,
    rangeInteractionPoint :: [Range]
  }
  deriving (Show, Generic)

newtype ErrorDetails = ErrorDetails
  { errorMessage :: Text
  }
  deriving (Show, Generic)

data GiveResult = GiveResult
  { giveResultContent :: GiveResultContent,
    interactionPointGiveResult :: InteractionPoint,
    kindGiveResult :: Text
  }
  deriving (Show, Generic)

newtype GiveResultContent = GiveResultContent
  { strGiveResultContent :: Text
  }
  deriving (Show, Generic)

data MakeCase = MakeCase
  { clausesMakeCase :: [Text],
    interactionPointMakeCase :: InteractionPoint,
    variantMakeCase :: Text -- "Function", "ExtendedLambda", etc.
  }
  deriving (Show, Generic)

instance FromJSON Response where
  parseJSON = withObject "Response" $ \v -> do
    kind' <- v .: "kind"
    case kind' :: String of
      "DisplayInfo" -> ResponseDisplay <$> parseJSON (Object v)
      "GiveAction" -> ResponseGive <$> parseJSON (Object v)
      "MakeCase" -> ResponseMakeCase <$> parseJSON (Object v)
      _ -> fail $ "Unknown response kind: " ++ kind'

instance FromJSON DisplayInfo where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON InfoContent where
  parseJSON = withObject "InfoContent" $ \o -> do
    kindVal <- o .: "kind"
    case kindVal :: String of
      "AllGoalsWarnings" ->
        AllGoalsWarnings
          <$> o .: "errors"
          <*> o .: "invisibleGoals"
          <*> o .: "visibleGoals"
          <*> o .: "warnings"
      "GoalSpecific" ->
        GoalSpecific
          <$> o .: "goalInfo"
          <*> o .: "interactionPoint"
      "Error" ->
        ErrorInfo
          <$> o .: "error"
          <*> o .: "warnings"
      "Auto" ->
        AutoFailed
          <$> o .: "info"
          <*> o .: "kind"
      _ -> pure (OtherInfo (Object o))

instance FromJSON VisibleGoal where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = fix}
    where
      fix "constraintObjVisible" = "constraintObj"
      fix "goalTypeVisible" = "type"
      fix "goalKindVisible" = "kind"
      fix s = s

instance FromJSON ConstraintObj where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = fix}
    where
      fix "idConstraintObj" = "id"
      fix "rangeConstraintObj" = "range"
      fix s = s

instance FromJSON Range where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = fix}
    where
      fix "startRange" = "start"
      fix "endRange" = "end"
      fix s = s

instance FromJSON Position where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = fix}
    where
      fix "colPosition" = "col"
      fix "linePosition" = "line"
      fix "posPosition" = "pos"
      fix s = s

instance FromJSON GoalInfo where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = fix}
    where
      fix "boundaryGoalInfo" = "boundary"
      fix "entriesGoalInfo" = "entries"
      fix "kindGoalInfo" = "kind"
      fix "outputFormsGoalInfo" = "outputForms"
      fix "rewriteGoalInfo" = "rewrite"
      fix "typeGoalInfo" = "type"
      fix "typeAuxGoalInfo" = "typeAux"
      fix s = s

instance FromJSON GoalEntry where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = fix}
    where
      fix "bindingGoalEntry" = "binding"
      fix "inScopeGoalEntry" = "inScope"
      fix "originalNameGoalEntry" = "originalName"
      fix "reifiedNameGoalEntry" = "reifiedName"
      fix s = s

instance FromJSON GoalTypeAux where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = fix}
    where
      fix "kindGoalTypeAux" = "kind"
      fix s = s

instance FromJSON InteractionPoint where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = fix}
    where
      fix "idInteractionPoint" = "id"
      fix "rangeInteractionPoint" = "range"
      fix s = s

instance FromJSON ErrorDetails where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = fix}
    where
      fix "errorMessage" = "message"
      fix s = s

instance FromJSON GiveResult where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = fix}
    where
      fix "giveResultContent" = "giveResult"
      fix "interactionPointGiveResult" = "interactionPoint"
      fix "kindGiveResult" = "kind"
      fix s = s

instance FromJSON GiveResultContent where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = fix}
    where
      fix "strGiveResultContent" = "str"
      fix s = s

instance FromJSON MakeCase where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = fix}
    where
      fix "clausesMakeCase" = "clauses"
      fix "interactionPointMakeCase" = "interactionPoint"
      fix "variantMakeCase" = "variant"
      fix s = s

parse :: ByteString -> Either String Response
parse = eitherDecode