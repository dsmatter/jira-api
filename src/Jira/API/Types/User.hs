{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Jira.API.Types.User where

import           Jira.API.Types.Avatar

import           Control.Lens
import           Data.Aeson

data User = User { _userName        :: String
                 , _userEmail       :: String
                 , _userAvatars     :: AvatarUrls
                 , _userDisplayName :: String
                 , _userIsActive    :: Bool
                 }

makeLenses ''User

instance Eq User where
  a == b = (a^.userName) == (b^.userName)

instance Ord User where
  a `compare` b = (a^.userDisplayName) `compare` (b^.userDisplayName)

instance Show User where
  show u = u^.userDisplayName

instance FromJSON User where
  parseJSON = withObject "Expected object" $ \o ->
    User <$> o .: "name"
         <*> o .: "emailAddress"
         <*> o .: "avatarUrls"
         <*> o .: "displayName"
         <*> o .: "active"
