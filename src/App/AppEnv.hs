{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE DuplicateRecordFields        #-}
{-# LANGUAGE TypeApplications       #-}

module App.AppEnv where

import Arbor.Logger                 (LogLevel, TimedFastLogger)
import Data.Generics.Product.Fields
import Data.Generics.Product.Typed
import GHC.Generics
import App.Options.Types
import Network.AWS                  (Env)

import qualified Arbor.Network.StatsD.Type as Z

data AppLogger = AppLogger
  { logger   :: TimedFastLogger
  , logLevel :: LogLevel
  } deriving Generic

data AppEnv o = AppEnv
  { options     :: o
  , awsEnv      :: Env
  , statsClient :: Z.StatsClient
  , logger      :: AppLogger
  } deriving Generic

instance {-# OVERLAPPING #-} HasType KafkaConfig a => HasType KafkaConfig (AppEnv a) where
  typed = field @"options" . typed

instance {-# OVERLAPPING #-} HasType AppLogger (AppEnv a) where
  typed = field @"logger"

instance {-# OVERLAPPING #-} HasType Z.StatsClient (AppEnv a) where
  typed = field @"statsClient"

instance {-# OVERLAPPING #-} HasType Env (AppEnv a) where
  typed = field @"awsEnv"
  