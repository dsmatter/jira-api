{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Jira.API.Types.Monad where

import           Jira.API.Types.Config

import           Control.Applicative
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.List
import qualified Data.Map                   as Map
import           Data.String.Conversions
import           Data.Typeable
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Network.HTTP.Types.Status

type ErrorMsg = String

data BadRequestInfo = BadRequestInfo [ErrorMsg] (Map.Map String String)
                    deriving (Eq)

instance Show BadRequestInfo where
  show (BadRequestInfo errorMessages errors) =
    unlines errorMessages ++ (unlines . map showError . Map.toList $ errors)
    where showError :: (String, ErrorMsg) -> String
          showError (scope, message) = scope ++ ": " ++ message

instance FromJSON BadRequestInfo where
  parseJSON = withObject "Expected object" $ \o ->
    BadRequestInfo <$> o .: "errorMessages"
                   <*> o .: "errors"

data JiraException = forall e. (Exception e) =>
                     OtherException e
                   | BadRequestException BadRequestInfo
                   | JsonFailure String
                   | GenericFailure
                   deriving (Typeable)

instance Show JiraException where
  show (OtherException e)         = "OtherException: " ++ show e
  show (JsonFailure r)            = "JSON Failure: " ++ show r
  show GenericFailure             = "Generic Failure"
  show (BadRequestException info) = "Bad Request: " ++ show info

instance Exception JiraException

badRequest :: ErrorMsg -> JiraException
badRequest msg = BadRequestException $ BadRequestInfo [msg] Map.empty

data JiraState = JiraState { jiraManager :: Manager
                           }

newtype JiraM a = JiraM { unJiraM :: ReaderT JiraConfig (StateT JiraState (EitherT JiraException IO)) a
                        } deriving ( Functor
                                   , Applicative
                                   , Monad
                                   , MonadReader JiraConfig
                                   , MonadState JiraState
                                   , MonadError JiraException
                                   , MonadIO
                                   )

instance MonadThrow JiraM where
  throwM = throwError . OtherException

-- Helper functions

runJira :: JiraConfig -> JiraM a -> IO (Either JiraException a)
runJira config m = do
  manager <- newManager tlsManagerSettings

  let unwrappedReader = runReaderT (unJiraM m) config
  let unwrappedState  = runStateT unwrappedReader (JiraState manager)
  runEitherT (fst <$> unwrappedState)

runJira' :: JiraConfig -> JiraM a -> IO a
runJira' config m = either (error . show) id <$> runJira config m

tryHttp :: IO a -> JiraM a
tryHttp m = try' m >>= either handleException return
  where
    handleException :: Exception e => e -> JiraM a
    handleException e = case fromException (toException e) of
      Nothing -> throwError $ OtherException e
      Just e  -> throwError $ parseHttpException e
    try' :: IO a -> JiraM (Either SomeException a)
    try' = liftIO . try

tryEither :: Exception e => Either e a -> JiraM a
tryEither = either throwM return

tryMaybe :: Exception e => e -> Maybe a -> JiraM a
tryMaybe ex = maybe (throwM ex) return

getConfig :: JiraM JiraConfig
getConfig = ask

getManager :: JiraM Manager
getManager = jiraManager <$> get

parseHttpException :: HttpException -> JiraException
parseHttpException e@(StatusCodeException status headers _) =
  case statusCode status of
    400 -> maybe (OtherException e) BadRequestException parseBody
    _   -> OtherException e
  where
    parseBody = decode =<< cs <$> findBody
    findBody  = snd <$> find ((== "X-Response-Body-Start") . fst) headers
parseHttpException e = OtherException e
