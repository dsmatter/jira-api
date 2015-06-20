{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE IncoherentInstances #-}

module Jira.API.Types.IssueType where

import           Jira.API.Types.Classes

import           Control.Applicative
import           Control.Lens           (makeLenses)
import           Data.Aeson

data IssueTypeIdentifier = IssueTypeId String
                         | IssueTypeName String
                           deriving (Show, Eq)

instance UrlIdentifier IssueTypeIdentifier where
  urlId (IssueTypeId s)   = s
  urlId (IssueTypeName s) = s

instance ToJSON IssueTypeIdentifier where
  toJSON (IssueTypeId s)   = object [ "id"   .= s ]
  toJSON (IssueTypeName s) = object [ "name" .= s ]

data IssueType = IssueType { _itId          :: String
                           , _itName        :: String
                           , _itDescription :: String
                           , _itIconUrl     :: String
                           , _itIsSubtask   :: Bool
                           } deriving (Eq, Show)

makeLenses ''IssueType

instance FromJSON IssueType where
  parseJSON = withObject "Expected object" $ \o ->
    IssueType <$> o .: "id"
              <*> o .: "name"
              <*> o .: "description"
              <*> o .: "iconUrl"
              <*> o .: "subtask"
