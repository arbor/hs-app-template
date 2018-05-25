{-# LANGUAGE FlexibleContexts #-}

module App.Commands.Service where

import App.AppEnv
import App.AppError
import App.Application
import App.AppState
import App.AWS.Env
import App.Commands.Types
import App.Conduit
import App.Kafka
import App.Options
import App.Stats
import Arbor.Logger
import Control.Lens
import Data.Conduit
import Data.Semigroup                       ((<>))
import HaskellWorks.Data.Conduit.Combinator
import Kafka.Avro                           (schemaRegistry)
import Kafka.Conduit.Sink
import Kafka.Conduit.Source
import Network.AWS.Types
import Options.Applicative
import System.Environment

import qualified App.Has     as H
import qualified App.Lens    as L
import qualified App.Service as Srv
import qualified Data.Text   as T

cmdService :: Mod CommandFields (IO ())
cmdService = command "service" $ flip info idm $ runService <$> optsService

runService :: CmdServiceOptions -> IO ()
runService opt = do
  progName <- T.pack <$> getProgName
  let logLvk    = opt ^. L.logLevel
  let statsConf = opt ^. L.statsConfig

  withStdOutTimedFastLogger $ \lgr -> do
    withStatsClient progName statsConf $ \stats -> do
      envAws <- mkEnv (opt ^. L.region) logLvk lgr
      let envApp = AppEnv opt envAws stats (AppLogger lgr logLvk)
      res <- runApplication envApp
      case res of
        Left err -> pushLogMessage lgr LevelError ("Exiting: " <> show err)
        Right _  -> pure ()
    pushLogMessage lgr LevelError ("Premature exit, must not happen." :: String)

runApplication ::
  ( H.HasKafkaConfig o
  , L.HasConsumerGroupId o ConsumerGroupId
  , L.HasInputTopic o TopicName
  , Show o)
  => AppEnv o
  -> IO (Either AppError AppState)
runApplication envApp =
  runApplicationM envApp $ do
    opt <- view H.appEnvOptions
    kafkaConf <- view H.kafkaConfig

    logInfo "Creating Kafka Consumer"
    consumer <- mkConsumer (opt ^. L.consumerGroupId) (opt ^. L.inputTopic)
    -- producer <- mkProducer -- Use this if you also want a producer.

    logInfo "Instantiating Schema Registry"
    sr <- schemaRegistry (kafkaConf ^. L.schemaRegistryAddress)

    logInfo "Running Kafka Consumer"
    runConduit $
      kafkaSourceNoClose consumer (kafkaConf ^. L.pollTimeoutMs)
      .| throwLeftSatisfyC KafkaErr isFatal                       -- throw any fatal error
      .| skipNonFatalExcept [isPollTimeout]                       -- discard any non-fatal except poll timeouts
      .| rightC (Srv.handleStream sr)                             -- handle messages (see Service.hs)
      .| everyNSeconds (kafkaConf ^. L.commitPeriodSec)  -- only commit ever N seconds, so we don't hammer Kafka.
      .| commitOffsetsSink consumer
      -- .| flushThenCommitSink consumer producer -- Swap with the above if you want a producer.

optsService :: Parser CmdServiceOptions
optsService = CmdServiceOptions
  <$> readOptionMsg "Valid values are LevelDebug, LevelInfo, LevelWarn, LevelError"
        (  long "log-level"
        <> metavar "LOG_LEVEL"
        <> showDefault <> value LevelInfo
        <> help "Log level.")
  <*> readOrFromTextOption
        (  long "region"
        <> metavar "AWS_REGION"
        <> showDefault <> value Oregon
        <> help "The AWS region in which to operate"
        )
  <*> ( TopicName <$> strOption
        (  long "input-topic"
        <> metavar "TOPIC"
        <> help "Input topic"))
  <*> ( ConsumerGroupId <$> strOption
    (  long "kafka-group-id"
    <> metavar "GROUP_ID"
    <> help "Kafka consumer group id"))
  <*> kafkaConfigParser
  <*> statsConfigParser
