{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module App.Has where

import App.AppEnv
import App.Options.Types
import Arbor.Logger          (LogLevel, TimedFastLogger)
import Control.Lens
import Control.Monad.Logger  (LogLevel (..))
import Data.Semigroup        ((<>))
import Data.Text             (Text)
import Kafka.Consumer.Types
import Kafka.Types
import Network.AWS           (Env, HasEnv (..))
import Network.AWS.Data.Text (FromText (..), fromText)
import Network.AWS.S3.Types  (Region (..))
import Network.Socket        (HostName)
import Network.StatsD        (SampleRate (..), StatsClient)
import Options.Applicative
import Text.Read             (readEither)

import qualified Data.Text   as T
import qualified Network.AWS as AWS

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


