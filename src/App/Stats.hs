{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module App.Stats where

import App.Application
import App.Options.Types
import Control.Exception
import Control.Lens
import Data.Generics.Product.Fields
import Data.Maybe                   (catMaybes)
import Data.Semigroup               ((<>))

import qualified Arbor.Network.StatsD      as S
import qualified Arbor.Network.StatsD.Type as Z

withStatsClient :: AppName -> StatsConfig -> (Z.StatsClient -> IO a) -> IO a
withStatsClient appName statsConf f = do
  globalTags <- mkStatsTags statsConf
  let statsOpts = Z.DogStatsSettings (statsConf ^. field @"host") (statsConf ^. field @"port")
  bracket (S.createStatsClient statsOpts (Z.MetricName appName) globalTags) S.closeStatsClient f

mkStatsTags :: StatsConfig -> IO [Z.Tag]
mkStatsTags statsConf = do
  deplId <- S.envTag "TASK_DEPLOY_ID" "deploy_id"
  let envTags = catMaybes [deplId]
  return $ envTags <> (statsConf ^. field @"tags" <&> toTag)
  where toTag (StatsTag (k, v)) = S.tag k v
