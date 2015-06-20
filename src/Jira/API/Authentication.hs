module Jira.API.Authentication ( applyAuth
                               , getOAuth

                               , module Jira.API.Authentication.Types
                               , module Jira.API.Authentication.KeyUtils
                               ) where

import           Jira.API.Authentication.KeyUtils
import           Jira.API.Authentication.Types
import           Jira.API.Types.Config

import           Control.Lens
import           Control.Monad.IO.Class
import           Crypto.Types.PubKey.RSA
import           Data.String.Conversions
import           Network.HTTP.Client
import           Web.Authenticate.OAuth

applyAuth :: MonadIO m => JiraConfig -> Request -> m Request
applyAuth config request =
  case config^.authentication of
   BasicAuthConfig user pass ->
     return $ applyBasicAuth (cs user) (cs pass) request
   OAuthConfig consumerKey privateKey accessToken accessTokenSecret ->
     let oauth       = getOAuth (config^.baseUrl) consumerKey privateKey
         credentials = newCredential (cs accessToken) (cs accessTokenSecret)
     in  signOAuth oauth credentials request

getOAuth :: String -> String -> PrivateKey -> OAuth
getOAuth baseUrl consumerKey privateKey =
  newOAuth { oauthServerName      = baseUrl
           , oauthRequestUri      = baseUrl ++ "/plugins/servlet/oauth/request-token"
           , oauthAuthorizeUri    = baseUrl ++ "/plugins/servlet/oauth/authorize"
           , oauthAccessTokenUri  = baseUrl ++ "/plugins/servlet/oauth/access-token"
           , oauthSignatureMethod = RSASHA1 privateKey
           , oauthConsumerKey     = cs consumerKey
           , oauthConsumerSecret  = cs ""
           }
