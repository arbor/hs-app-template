{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module App.AppEnv where

import Arbor.Logger   (LogLevel, TimedFastLogger)
import Network.AWS    (Env)
import Network.StatsD (StatsClient)

data AppLogger = AppLogger
  { _appLoggerLogger   :: TimedFastLogger
  , _appLoggerLogLevel :: LogLevel
  }

data AppEnv o = AppEnv
  { _appEnvOptions     :: o
  , _appEnvAwsEnv      :: Env
  , _appEnvStatsClient :: StatsClient
  , _appEnvLogger      :: AppLogger
  }
