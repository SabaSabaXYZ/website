module Main where

import Configuration (ServerConfiguration(..), defaultConfiguration, readConfiguration)
import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import Data.Either (either)
import Data.Maybe (maybe, listToMaybe)
import Network.Wai.Handler.Warp (run)
import Server
import ServerMonad (ConfigMonad(..), ServerMonad(..))
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = runConfigMonad getConfiguration >>= flip run app . configPort

getConfiguration :: ConfigMonad ServerConfiguration
getConfiguration = do
  configFilePath <- liftIO getArgs >>= pure . (readMaybe <=< listToMaybe)
  maybe (pure $ Right defaultConfiguration) readConfiguration configFilePath >>= pure . either error id
