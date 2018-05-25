module App.Options.Types where

import Control.Monad.Logger (LogLevel (..))
import Data.Text            (Text)
import Kafka.Consumer.Types
import Kafka.Types
import Network.AWS.S3.Types (Region (..))
import Network.Socket       (HostName)
import Network.StatsD       (SampleRate (..))

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

data Options = Options
  { _optionsLogLevel        :: LogLevel
  , _optionsRegion          :: Region
  , _optionsInputTopic      :: TopicName
  , _optionsConsumerGroupId :: ConsumerGroupId
  , _optionsKafkaConfig     :: KafkaConfig
  , _optionsStatsConfig     :: StatsConfig
  } deriving (Show)
