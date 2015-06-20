{-# LANGUAGE TemplateHaskell #-}

module Jira.API.Types.Config where

import           Jira.API.Authentication.Types

import           Control.Lens

data JiraConfig = JiraConfig { _baseUrl        :: String
                             , _authentication :: AuthConfig
                             } deriving (Show, Eq)

makeLenses ''JiraConfig
