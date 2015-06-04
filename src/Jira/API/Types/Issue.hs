{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Jira.API.Types.Issue where

import Jira.API.Types.Project
import Jira.API.Types.Classes

import Control.Applicative
import Control.Lens (makeLenses, (^.))
import Data.Aeson

data IssueIdentifier = IssueId Int
                     | IssueKey String
                       deriving (Show, Eq)

instance UrlIdentifier IssueIdentifier where
  urlId (IssueId n)  = show n
  urlId (IssueKey s) = s

data IssueTypeIdentifier = IssueTypeId String
                         | IssueTypeName String
                           deriving (Show, Eq)

instance UrlIdentifier IssueTypeIdentifier where
  urlId (IssueTypeId s)   = s
  urlId (IssueTypeName s) = s

instance ToJSON IssueTypeIdentifier where
  toJSON (IssueTypeId s)   = object [ "id"   .= s ]
  toJSON (IssueTypeName s) = object [ "name" .= s ]

data IssueCreationData = IssueCreationData { _icProject :: ProjectIdentifier
                                           , _icType    :: IssueTypeIdentifier
                                           , _icSummary :: String
                                           } deriving (Show, Eq)

makeLenses ''IssueCreationData

instance ToJSON IssueCreationData where
 toJSON issueCreation = object [ "fields" .= fields ]
   where fields = object [ "project"   .= (issueCreation^.icProject)
                         , "issuetype" .= (issueCreation^.icType)
                         , "summary"   .= (issueCreation^.icSummary)
                         ]

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

data Issue = Issue { _iId          :: String
                   , _iKey         :: String
                   , _iType        :: IssueType
                   , _iProject     :: Project
                   , _iSummary     :: String
                   , _iDescription :: String
                   } deriving (Eq, Show)

makeLenses ''Issue

instance FromJSON Issue where
  parseJSON = withObject "Expected object" $ \o -> do
    fields <- o .: "fields"
    Issue <$> o .: "id"
          <*> o .: "key"
          <*> fields .: "issuetype"
          <*> fields .: "project"
          <*> fields .: "summary"
          <*> fields .:? "description" .!= ""


newtype IssuesResponse = IssuesResponse [Issue]

instance FromJSON IssuesResponse where
  parseJSON = withObject "Expected object" $ \o -> do
    IssuesResponse <$> o .: "issues"
