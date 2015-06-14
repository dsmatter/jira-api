{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Jira.API.Types.Project where

import           Jira.API.Types.Avatar
import           Jira.API.Types.Classes
import           Jira.API.Types.IssueType

import           Control.Applicative
import           Control.Lens             (makeLenses)
import           Data.Aeson

type ProjectKey = String

data ProjectIdentifier = ProjectId String
                       | ProjectKey String
                         deriving (Show, Eq)

instance UrlIdentifier ProjectIdentifier where
  urlId (ProjectId s)  = s
  urlId (ProjectKey s) = s

instance ToJSON ProjectIdentifier where
  toJSON (ProjectId s)  = object [ "id"  .= s ]
  toJSON (ProjectKey s) = object [ "key" .= s ]

data Project = Project { _pId         :: String
                       , _pKey        :: String
                       , _pName       :: String
                       , _pAvatars    :: AvatarUrls
                       } deriving (Eq, Show)

instance FromJSON Project where
  parseJSON = withObject "Expected Object" $ \o ->
    Project <$> o .: "id"
            <*> o .: "key"
            <*> o .: "name"
            <*> o .: "avatarUrls"

makeLenses ''Project
