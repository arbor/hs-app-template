module Main where

import App.Commands
import Control.Monad
import Options.Applicative

main :: IO ()
main = join
  $ customExecParser (prefs $ mconcat [showHelpOnEmpty, showHelpOnError])
  $ info (commandsParser <**> helper)
  $ mconcat
    [ fullDesc
    , progDesc "Template for Haskell projects with built-in support for Kafka and AWS"
    , header "Haskell App Template"
    ]
