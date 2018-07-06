{-# LANGUAGE DuplicateRecordFields #-}

module App.Commands.Types where

import App.Options.Types
import Control.Monad.Logger (LogLevel (..))
import GHC.Generics
import Kafka.Consumer.Types
import Kafka.Types
import Network.AWS.S3.Types (Region (..))

data CmdDevRunOptions = CmdDevRunOptions deriving (Show, Generic)

data CmdServiceOptions = CmdServiceOptions
  { logLevel        :: LogLevel
  , region          :: Region
  , inputTopic      :: TopicName
  , consumerGroupId :: ConsumerGroupId
  , kafkaConfig     :: KafkaConfig
  , statsConfig     :: StatsConfig
  } deriving (Show, Generic)
