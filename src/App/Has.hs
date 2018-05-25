{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module App.Has where

import App.AppEnv
import App.Options.Types
import Control.Lens
import Network.AWS       (HasEnv (..))
import Network.StatsD    (StatsClient)

makeClassy ''AppEnv
makeClassy ''AppLogger
makeClassy ''CmdServiceOptions
makeClassy ''KafkaConfig
makeClassy ''StatsConfig

instance HasKafkaConfig CmdServiceOptions where
  kafkaConfig = cmdServiceOptionsKafkaConfig

instance HasStatsConfig CmdServiceOptions where
  statsConfig = cmdServiceOptionsStatsConfig

instance HasEnv (AppEnv o) where
  environment = appEnv . appEnvAwsEnv

class HasStatsClient a where
  statsClient :: Lens' a StatsClient

instance HasStatsClient StatsClient where
  statsClient = id

instance HasStatsClient (AppEnv o) where
  statsClient = appEnvStatsClient

instance HasAppLogger (AppEnv o) where
  appLogger = appEnv . appLogger

instance HasKafkaConfig o => HasKafkaConfig (AppEnv o) where
  kafkaConfig = appEnvOptions . kafkaConfig

instance HasStatsConfig o => HasStatsConfig (AppEnv o) where
  statsConfig = appEnvOptions . statsConfig
