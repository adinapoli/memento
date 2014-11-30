{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Memento.Types 
  ( Memento(..)
  , runMemento
  , startMemento
  , lookupEnv
  , requireEnv
  , newMementoEnv
  , liftIO 
  ) where

import Control.Monad.State
import Memento.Config
import Control.Applicative
import qualified Data.Text as T
import Data.Configurator.Types
import Data.Configurator as Cfg

--------------------------------------------------------------------------------
data MementoEnv = MementoEnv {
  config :: Config
  }

newtype Memento a = Memento {
  unMemento :: StateT MementoEnv IO a
  } deriving (Functor, Applicative, Monad, MonadState MementoEnv, MonadIO)

--------------------------------------------------------------------------------
newMementoEnv :: IO MementoEnv
newMementoEnv = do
  checkConfig
  home <- userHomePath
  MementoEnv <$> load [Required $ T.unpack $ mementoCfgPath home]

--------------------------------------------------------------------------------
lookupEnv :: Configured a => Name -> Memento (Maybe a)
lookupEnv key = do
  env <- get
  liftIO $ Cfg.lookup (config env) key

--------------------------------------------------------------------------------
requireEnv :: Configured a => Name -> Memento a
requireEnv key = do
  env <- get
  liftIO $ Cfg.require (config env) key

--------------------------------------------------------------------------------
runMemento :: MementoEnv -> Memento () -> IO ()
runMemento env action = flip evalStateT env $ unMemento action

--------------------------------------------------------------------------------
startMemento :: Memento () -> IO ()
startMemento action = do
  env <- newMementoEnv
  runMemento env action
