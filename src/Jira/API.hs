{-# LANGUAGE OverloadedStrings #-}

module Jira.API ( createIssue
                , getIssue
                , deleteIssue

                , getJson
                , postJson
                , putJson
                , getRaw
                , deleteRaw
                , postRaw
                , putRaw
                , sendRequest
                , urlWithPath

                , module Jira.API.Types
                , module Jira.API.Authentication
                ) where

import Jira.API.Authentication
import Jira.API.Types

import Control.Applicative
import Control.Lens
import Control.Monad.Error
import Data.Aeson
import Data.Aeson.Lens
import Data.String.Conversions
import Network.HTTP.Client

import qualified Data.ByteString.Lazy as LBS

-- Typed API Layer

createIssue :: IssueCreationData -> JiraM String
createIssue = takeKey <=< postJson "/issue"
  where takeKey :: Value -> JiraM String
        takeKey v = tryMaybe (keyNotFound v) $ v ^? key "key" . asString
        keyNotFound v = JsonFailure $ "Key not found in JSON: " ++ cs (encode v)
        asString = _String . to cs

getIssue :: IssueIdentifier -> JiraM Issue
getIssue = getJson . issuePath

deleteIssue :: IssueIdentifier -> JiraM ()
deleteIssue = deleteRaw . issuePath

issuePath :: IssueIdentifier -> String
issuePath issue = "/issue/" ++ urlId issue

-- Generic JSON API Layer

getJson :: FromJSON a => String -> JiraM a
getJson = decodeJson <=< getRaw

postJson :: (FromJSON a, ToJSON p) => String -> p -> JiraM a
postJson urlPath = decodeJson <=< postRaw urlPath . encode

putJson :: (FromJSON a, ToJSON p) => String -> p -> JiraM a
putJson urlPath = decodeJson <=< putRaw urlPath . encode

decodeJson :: FromJSON a => LBS.ByteString -> JiraM a
decodeJson rawJson = case decode rawJson of
   Nothing -> throwError $ JsonFailure (cs rawJson)
   Just v  -> return v

-- Raw API Layer

getRaw :: String -> JiraM LBS.ByteString
getRaw = raw' "GET"

deleteRaw :: String -> JiraM ()
deleteRaw = void . raw' "DELETE"

postRaw :: String -> LBS.ByteString -> JiraM LBS.ByteString
postRaw urlPath payload = raw "POST" urlPath (addJsonPayload payload)

putRaw :: String -> LBS.ByteString -> JiraM LBS.ByteString
putRaw urlPath payload = raw "PUT" urlPath (addJsonPayload payload)

raw' :: String -> String -> JiraM LBS.ByteString
raw' httpMethod urlPath = raw httpMethod urlPath id

raw :: String -> String -> (Request -> Request) -> JiraM LBS.ByteString
raw httpMethod urlPath transformRequest = do
  url         <- urlWithPath urlPath
  initRequest <- parseUrl url
  let request = transformRequest initRequest { method = cs httpMethod }
  responseBody <$> sendRequest request

-- Helpers

sendRequest :: Request -> JiraM (Response LBS.ByteString)
sendRequest request = do
  config <- getConfig
  manager <- getManager
  authedRequest <- applyAuth config request
  tryIO $ httpLbs authedRequest manager

urlWithPath :: String -> JiraM String
urlWithPath urlPath = view baseUrl <$$> (++ "/rest/api/2/" ++ removeFirstSlash urlPath)
  where removeFirstSlash ('/':xs) = xs
        removeFirstSlash s        = s
        (<$$>) = flip (<$>)

addJsonPayload :: LBS.ByteString -> Request -> Request
addJsonPayload payload r =
  r { requestBody    = RequestBodyLBS payload
    , requestHeaders = [("Content-Type", "application/json;charset=utf-8")]
    }
