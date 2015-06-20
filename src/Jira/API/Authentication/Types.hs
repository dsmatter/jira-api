{-# LANGUAGE TemplateHaskell #-}

module Jira.API.Authentication.Types where

import           Control.Lens
import           Crypto.Types.PubKey.RSA

data AuthConfig = BasicAuthConfig { _username   :: String
                                  , _passphrase :: String
                                  }
                | OAuthConfig     { _consumerKey       :: String
                                  , _signingKey        :: PrivateKey
                                  , _accessToken       :: String
                                  , _accessTokenSecret :: String
                                  } deriving (Eq, Show)

makeLenses ''AuthConfig
