{-# LANGUAGE OverloadedStrings #-}

module Reply (Command (..), parse) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString.Lazy (ByteString)

data Command
  = Give Int String
  | Auto Int
  | CaseSplit Int String
  | AddBinders [String] String
  | Reset
  deriving (Show, Eq)

instance FromJSON Command where
  -- Expect the top-level object that has a "reply" field which is the actual command object
  parseJSON = withObject "Reply" $ \o -> do
    -- parse "reply" as an Object
    replyObj <- (o .: "reply" :: Parser Object)
    parseCommand replyObj

-- Helper that parses the inner command object
parseCommand :: Object -> Parser Command
parseCommand o = do
  cmd <- o .: "command" :: Parser String
  case cmd of
    "GIVE" -> Give <$> o .: "hole" <*> o .: "expression"
    "AUTO" -> Auto <$> o .: "hole"
    "CASE_SPLIT" -> CaseSplit <$> o .: "hole" <*> o .: "binder"
    "ADD_BINDERS" -> AddBinders <$> o .: "binders" <*> o .: "function"
    "RESET" -> pure Reset
    other -> fail $ "Unknown command: " ++ other

parse :: ByteString -> Either String Command
parse = eitherDecode