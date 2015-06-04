{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Jira.API.Types.Transition where

import           Control.Lens (makeLenses)
import           Data.Aeson
import           Data.Aeson.TH

data Transition = Transition { _transitionId   :: String
                             , _transitionName :: String
                             } deriving (Show)

makeLenses ''Transition

instance FromJSON Transition where
  parseJSON = withObject "Expected object" $ \o ->
    Transition <$> o .: "id" <*> o .: "name"

data TransitionIdentifier = TransitionId String
                          | TransitionName String
                          deriving (Show)

newtype TransitionIdRequest = TransitionIdRequest String

instance ToJSON TransitionIdRequest where
  toJSON (TransitionIdRequest tid) = object [ "id" .= tid ]

newtype TransitionsResponse = TransitionsResponse [Transition]

instance FromJSON TransitionsResponse where
  parseJSON = withObject "Expected object" $ \o ->
    TransitionsResponse <$> o .: "transitions"
