module App.Options.Types where

import Data.Text      (Text)
import Kafka.Types
import Network.Socket (HostName)
import Network.StatsD (SampleRate (..))

newtype StatsTag = StatsTag (Text, Text) deriving (Show, Eq)

data KafkaConfig = KafkaConfig
  { _kafkaConfigBroker                :: BrokerAddress
  , _kafkaConfigSchemaRegistryAddress :: String
  , _kafkaConfigPollTimeoutMs         :: Timeout
  , _kafkaConfigQueuedMaxMsgKBytes    :: Int
  , _kafkaConfigDebugOpts             :: String
  , _kafkaConfigCommitPeriodSec       :: Int
  } deriving (Show)

data StatsConfig = StatsConfig
  { _statsConfigHost       :: HostName
  , _statsConfigPort       :: Int
  , _statsConfigTags       :: [StatsTag]
  , _statsConfigSampleRate :: SampleRate
  } deriving (Show)
