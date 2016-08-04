{-# LANGUAGE OverloadedStrings #-}

module Jira.API.Types.Assignee where

import           Data.Aeson

data Assignee = AssigneeUsername String
              | AssigneeAutomatic
              | AssigneeNobody

instance ToJSON Assignee where
  toJSON (AssigneeUsername name)  = object [ "name" .= name ]
  toJSON AssigneeAutomatic        = object [ "name" .= ("-1" :: String) ]
  toJSON AssigneeNobody           = object [ "name" .= Null ]
