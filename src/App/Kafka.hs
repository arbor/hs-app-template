{-# LANGUAGE MultiParamTypeClasses #-}
module App.Kafka
  ( ConsumerGroupSuffix(..), TopicName(..)
  , KafkaConsumer, KafkaProducer, Timeout(..)
  , mkConsumer
  , mkProducer
  ) where

import Arbor.Logger
import Control.Lens                 hiding (cons)
import Control.Monad                (void)
import Control.Monad.Logger         (LogLevel (..))
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Foldable
import Data.List.Split
import Data.Monoid                  ((<>))
import Kafka.Conduit.Sink           as KSnk
import Kafka.Conduit.Source         as KSrc

import qualified App.Has  as H
import qualified App.Lens as L
import qualified Data.Map as M

newtype ConsumerGroupSuffix = ConsumerGroupSuffix String deriving (Show, Eq)

mkConsumer :: (MonadResource m, MonadReader r m, H.HasKafkaConfig r, H.HasAppLogger r)
            => ConsumerGroupId
            -> TopicName
            -> m KafkaConsumer
mkConsumer cgid topic = do
  conf <- view H.kafkaConfig
  logs <- view H.appLogger
  let props = fold
        [ KSrc.brokersList [conf ^. L.broker]
        , cgid & groupId
        , conf ^. L.queuedMaxMsgKBytes & queuedMaxMessagesKBytes
        , noAutoCommit
        , KSrc.suppressDisconnectLogs
        , KSrc.logLevel (kafkaLogLevel (logs ^. L.logLevel))
        , KSrc.debugOptions (kafkaDebugEnable (conf ^. L.debugOpts))
        , KSrc.setCallback (logCallback   (\l s1 s2 -> pushLogMessage (logs ^. L.logger) (kafkaLogLevelToLogLevel $ toEnum l) ("[" <> s1 <> "] " <> s2)))
        , KSrc.setCallback (errorCallback (\e s -> pushLogMessage (logs ^. L.logger) LevelError ("[" <> show e <> "] " <> s)))
        ]
      sub = topics [topic] <> offsetReset Earliest
      cons = newConsumer props sub >>= either throwM return
  snd <$> allocate cons (void . closeConsumer)

mkProducer :: (MonadResource m, MonadReader r m, H.HasKafkaConfig r, H.HasAppLogger r) => m KafkaProducer
mkProducer = do
  conf <- view H.kafkaConfig
  logs <- view H.appLogger
  let props = KSnk.brokersList [conf ^. L.broker]
           <> KSnk.suppressDisconnectLogs
           <> KSnk.sendTimeout (Timeout 0) -- message sending timeout, 0 means "no timeout"
           <> KSnk.logLevel (kafkaLogLevel (logs ^. L.logLevel))
           <> KSnk.setCallback (logCallback   (\l s1 s2 -> pushLogMessage (logs ^. L.logger) (kafkaLogLevelToLogLevel $ toEnum l) ("[" <> s1 <> "] " <> s2)))
           <> KSnk.setCallback (errorCallback (\e s -> pushLogMessage (logs ^. L.logger) LevelError ("[" <> show e <> "] " <> s)))
           <> KSnk.setCallback (deliveryErrorsCallback (logAndDieHard (logs ^. L.logger)))
           <> KSnk.extraProps (M.singleton "linger.ms"                 "100")
           <> KSnk.extraProps (M.singleton "message.send.max.retries"  "0"  )
           <> KSnk.compression Gzip
      prod = newProducer props >>= either throwM return
  snd <$> allocate prod closeProducer

logAndDieHard :: TimedFastLogger -> KafkaError -> IO ()
logAndDieHard lgr err = do
  let errMsg = "Producer is unable to deliver messages: " <> show err
  pushLogMessage lgr LevelError errMsg
  error errMsg

kafkaLogLevel :: LogLevel -> KafkaLogLevel
kafkaLogLevel l = case l of
  LevelDebug   -> KafkaLogDebug
  LevelInfo    -> KafkaLogInfo
  LevelWarn    -> KafkaLogWarning
  LevelError   -> KafkaLogErr
  LevelOther _ -> KafkaLogCrit

kafkaLogLevelToLogLevel :: KafkaLogLevel -> LogLevel
kafkaLogLevelToLogLevel l = case l of
  KafkaLogDebug   -> LevelDebug
  KafkaLogInfo    -> LevelInfo
  KafkaLogWarning -> LevelWarn
  KafkaLogErr     -> LevelError
  KafkaLogCrit    -> LevelError
  KafkaLogAlert   -> LevelWarn
  KafkaLogNotice  -> LevelInfo
  KafkaLogEmerg   -> LevelError

kafkaDebugEnable :: String -> [KafkaDebug]
kafkaDebugEnable str = map debug (splitWhen (== ',') str)
  where
    debug :: String -> KafkaDebug
    debug m = case m of
      "generic"  -> DebugGeneric
      "broker"   -> DebugBroker
      "topic"    -> DebugTopic
      "metadata" -> DebugMetadata
      "queue"    -> DebugQueue
      "msg"      -> DebugMsg
      "protocol" -> DebugProtocol
      "cgrp"     -> DebugCgrp
      "security" -> DebugSecurity
      "fetch"    -> DebugFetch
      "feature"  -> DebugFeature
      "all"      -> DebugAll
      _          -> DebugGeneric
