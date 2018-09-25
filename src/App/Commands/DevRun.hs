{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module App.Commands.DevRun where

import App.AWS.Env
import App.Commands.Types
import App.Orphans                  ()
import Arbor.Logger
import Arbor.Network.StatsD         (MonadStats (..))
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger         (LoggingT, MonadLogger)
import Control.Monad.Trans.Resource
import Network.AWS                  as AWS hiding (LogLevel)
import Options.Applicative

import qualified Arbor.Network.StatsD.Type as Z

cmdDevRun :: Mod CommandFields (IO ())
cmdDevRun = command "dev-run" $ flip info idm $ runDevRun <$> optsDevRun

newtype DevApp a = DevApp
  { unDevApp :: (LoggingT AWS) a
  } deriving ( Functor, Applicative, Monad, MonadIO, MonadBase IO, MonadThrow, MonadCatch
             , MonadMask, MonadAWS, MonadLogger, MonadResource)

instance MonadStats DevApp where
  getStatsClient = pure Z.Dummy

runDevRun :: CmdDevRunOptions -> IO ()
runDevRun _ = withStdOutTimedFastLogger $ \logger -> do
  env <- mkEnv Oregon LevelInfo logger
  runDevRun' env logger $ return ()

runDevRun' :: HasEnv e => e -> TimedFastLogger -> DevApp a -> IO a
runDevRun' e logger f = runResourceT . runAWS e $ runTimedLogT LevelInfo logger (unDevApp f)

optsDevRun :: Parser CmdDevRunOptions
optsDevRun = pure CmdDevRunOptions
