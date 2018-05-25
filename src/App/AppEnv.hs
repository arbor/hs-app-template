{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module App.AppEnv where

import App.Options
import Arbor.Logger   (LogLevel, TimedFastLogger)
import Control.Lens
import Network.AWS    (Env, HasEnv (..))
import Network.StatsD (StatsClient)

data AppLogger = AppLogger
  { _appLoggerLogger   :: TimedFastLogger
  , _appLoggerLogLevel :: LogLevel
  }

data AppEnv = AppEnv
  { _appEnvOptions     :: Options
  , _appEnvAwsEnv      :: Env
  , _appEnvStatsClient :: StatsClient
  , _appEnvLogger      :: AppLogger
  }

makeClassy ''AppLogger
makeClassy ''AppEnv

instance HasEnv AppEnv where
  environment = appEnv . appEnvAwsEnv

class HasStatsClient a where
  statsClient :: Lens' a StatsClient

instance HasStatsClient StatsClient where
  statsClient = id

instance HasStatsClient AppEnv where
  statsClient = appEnvStatsClient

instance HasAppLogger AppEnv where
  appLogger = appEnv . appLogger

instance HasKafkaConfig AppEnv where
  kafkaConfig = appEnvOptions . kafkaConfig

instance HasStatsConfig AppEnv where
  statsConfig = appEnvOptions . statsConfig

