module App.Commands.Types where

import App.Options.Types
import Control.Monad.Logger (LogLevel (..))
import Kafka.Consumer.Types
import Kafka.Types
import Network.AWS.S3.Types (Region (..))

data CmdDevRunOptions = CmdDevRunOptions deriving Show

data CmdServiceOptions = CmdServiceOptions
  { _cmdServiceOptionsLogLevel        :: LogLevel
  , _cmdServiceOptionsRegion          :: Region
  , _cmdServiceOptionsInputTopic      :: TopicName
  , _cmdServiceOptionsConsumerGroupId :: ConsumerGroupId
  , _cmdServiceOptionsKafkaConfig     :: KafkaConfig
  , _cmdServiceOptionsStatsConfig     :: StatsConfig
  } deriving Show
