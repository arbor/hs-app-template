module Main where

import App.Commands
import Control.Monad
import Data.Semigroup      ((<>))
import Options.Applicative

main :: IO ()
main = join
  $ customExecParser (prefs $ showHelpOnEmpty <> showHelpOnError)
  $ info (commandsParser <**> helper)
  $ (  fullDesc
    <> progDesc "Template for Haskell projects with built-in support for Kafka and AWS"
    <> header "Haskell App Template"
    )
