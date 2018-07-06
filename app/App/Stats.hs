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
import Network.StatsD               as S

withStatsClient :: AppName -> StatsConfig -> (StatsClient -> IO a) -> IO a
withStatsClient appName statsConf f = do
  globalTags <- mkStatsTags statsConf
  let statsOpts = DogStatsSettings (statsConf ^. field @"host") (statsConf ^. field @"port")
  bracket (createStatsClient statsOpts (MetricName appName) globalTags) closeStatsClient f

mkStatsTags :: StatsConfig -> IO [Tag]
mkStatsTags statsConf = do
  deplId <- envTag "TASK_DEPLOY_ID" "deploy_id"
  let envTags = catMaybes [deplId]
  return $ envTags <> (statsConf ^. field @"tags" <&> toTag)
  where toTag (StatsTag (k, v)) = S.tag k v
