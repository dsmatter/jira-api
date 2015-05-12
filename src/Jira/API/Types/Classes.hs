module Jira.API.Types.Classes where

class UrlIdentifier a where
  urlId :: a -> String
