{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}

module App.S3Spec
  ( spec
  ) where

import App.AWS.S3
import Control.Arrow               (returnA)
import Control.Lens
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.AWS
import Data.ByteString             (ByteString)
import Data.Conduit
import Data.Monoid
import Data.Monoid
import Data.Text                   (Text)
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Control.Monad.IO.Class as IO
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.Conduit.List      as CL
import qualified Data.Foldable          as Fold
import qualified Data.Text.IO           as Text
import qualified System.IO              as IO

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: redundant bracket"          :: String) #-}

spec :: Spec
spec = describe "Arbor.Event.Reactor.S3Spec" $ do
  describe "dependenciesTable" $ do
    it "insert into table" $ require $ withTests 1 $ property $ do
      lgr <- liftIO $ newLogger Trace IO.stdout
      env <- liftIO $ newEnv Discover <&> set envLogger lgr . set envRegion Oregon
      liftIO $ runResourceT . runAWST env $ do
        !bs <- LBS.toStrict <$> downloadLBS "jky-mayhem" "netsuite.json"
        liftIO $ IO.print bs

      True === True
