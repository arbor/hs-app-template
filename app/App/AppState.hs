module App.AppState where

import GHC.Generics

data AppState = AppState {} deriving (Show, Generic)

appStateEmpty :: AppState
appStateEmpty = AppState {}
