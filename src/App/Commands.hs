module App.Commands where

import App.Commands.Service
import Data.Semigroup       ((<>))
import Options.Applicative

commandsParser :: Parser (IO ())
commandsParser = subparser $ mempty
  <>  cmdService
