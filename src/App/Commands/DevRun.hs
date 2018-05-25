{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module App.Commands.DevRun where

import App.AWS.Env
import App.Commands.Types
import App.Orphans                  ()
import Arbor.Logger
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger         (LoggingT, MonadLogger)
import Control.Monad.Trans.Resource
import Network.AWS                  as AWS hiding (LogLevel)
import Network.StatsD               as S
import Options.Applicative

cmdDevRun :: Mod CommandFields (IO ())
cmdDevRun = command "dev-run" $ flip info idm $ runDevRun <$> optsDevRun

newtype DevApp a = DevApp
  { unDevApp :: (LoggingT AWS) a
  } deriving ( Functor, Applicative, Monad, MonadIO, MonadBase IO, MonadThrow, MonadCatch
             , MonadMask, MonadAWS, MonadLogger, MonadResource)

instance MonadStats DevApp where
  getStatsClient = pure Dummy

runDevRun :: CmdDevRunOptions -> IO ()
runDevRun _ = do
  withStdOutTimedFastLogger $ \logger -> do
    env <- mkEnv Oregon LevelInfo logger
    runDevRun' env logger $ do
      return ()

runDevRun' :: HasEnv e => e -> TimedFastLogger -> DevApp a -> IO a
runDevRun' e logger f = runResourceT . runAWS e $ runTimedLogT LevelInfo logger (unDevApp f)

optsDevRun :: Parser CmdDevRunOptions
optsDevRun = pure CmdDevRunOptions
