{-# LANGUAGE DuplicateRecordFields #-}

module App.Options.Types where

import Data.Text      (Text)
import GHC.Generics
import Kafka.Types
import Network.Socket (HostName)
import Network.StatsD (SampleRate (..))

newtype StatsTag = StatsTag (Text, Text) deriving (Show, Eq)

data KafkaConfig = KafkaConfig
  { broker                :: BrokerAddress
  , schemaRegistryAddress :: String
  , pollTimeoutMs         :: Timeout
  , queuedMaxMsgKBytes    :: Int
  , debugOpts             :: String
  , commitPeriodSec       :: Int
  } deriving (Show, Generic)

data StatsConfig = StatsConfig
  { host       :: HostName
  , port       :: Int
  , tags       :: [StatsTag]
  , sampleRate :: SampleRate
  } deriving (Show, Generic)
