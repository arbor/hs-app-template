{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module App.Lens where

import App.AppEnv
import App.Options.Types
import Control.Lens

makeFields ''AppEnv
makeFields ''AppLogger
makeFields ''CmdServiceOptions
makeFields ''KafkaConfig
makeFields ''StatsConfig
