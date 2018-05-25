{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module App.Has where

import App.AppEnv
import App.Options.Types
import Control.Lens
import Network.AWS       (HasEnv (..))
import Network.StatsD    (StatsClient)

makeClassy ''AppEnv
makeClassy ''AppLogger
makeClassy ''KafkaConfig
makeClassy ''Options
makeClassy ''StatsConfig

instance HasKafkaConfig Options where
  kafkaConfig = optionsKafkaConfig

instance HasStatsConfig Options where
  statsConfig = optionsStatsConfig

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
