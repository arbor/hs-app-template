module App.Stats where

import App.Application
import App.Options.Types
import Control.Exception
import Control.Lens
import Data.Maybe        (catMaybes)
import Data.Semigroup    ((<>))
import Network.StatsD    as S

import qualified App.Lens as L

withStatsClient :: AppName -> StatsConfig -> (StatsClient -> IO a) -> IO a
withStatsClient appName statsConf f = do
  globalTags <- mkStatsTags statsConf
  let statsOpts = DogStatsSettings (statsConf ^. L.host) (statsConf ^. L.port)
  bracket (createStatsClient statsOpts (MetricName appName) globalTags) closeStatsClient f

mkStatsTags :: StatsConfig -> IO [Tag]
mkStatsTags statsConf = do
  deplId <- envTag "TASK_DEPLOY_ID" "deploy_id"
  let envTags = catMaybes [deplId]
  return $ envTags <> (statsConf ^. L.tags <&> toTag)
  where toTag (StatsTag (k, v)) = S.tag k v
