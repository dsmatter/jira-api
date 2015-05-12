{-# LANGUAGE GeneralizedNewtypeDeriving
           , DeriveDataTypeable
           , ExistentialQuantification
           , ScopedTypeVariables #-}

module Jira.API.Types.Monad where

import Jira.API.Types.Config

import Control.Applicative
import Control.Monad.Catch
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Either
import Data.Typeable
import Network.HTTP.Client

data JiraException = forall e. (Exception e) =>
                     OtherException e
                   | JsonFailure String
                   | GenericFailure
                   deriving (Typeable)

instance Show JiraException where
  show (OtherException e) = "OtherException: " ++ show e
  show (JsonFailure r)    = "JSON Failure: " ++ show r
  show GenericFailure     = "Generic Failure"

instance Exception JiraException

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
  manager <- newManager defaultManagerSettings

  let unwrappedReader = runReaderT (unJiraM m) config
  let unwrappedState  = runStateT unwrappedReader (JiraState manager)
  runEitherT (fst <$> unwrappedState)

runJira' :: JiraConfig -> JiraM a -> IO a
runJira' config m = either (error . show) id <$> runJira config m

tryIO :: IO a -> JiraM a
tryIO m = try' m >>= tryEither
    where try' :: IO a -> JiraM (Either SomeException a)
          try' = liftIO . try

tryEither :: Exception e => Either e a -> JiraM a
tryEither = either throwM return

tryMaybe :: Exception e => e -> Maybe a -> JiraM a
tryMaybe ex = maybe (throwM ex) return

getConfig :: JiraM JiraConfig
getConfig = ask

getManager :: JiraM Manager
getManager = jiraManager <$> get
