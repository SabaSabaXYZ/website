module Main where

import Configuration (ServerConfiguration(..), defaultConfiguration, readConfiguration)
import Control.Monad.IO.Class (liftIO)
import Data.Either (either)
import Data.Maybe (maybe, listToMaybe)
import Network.Wai.Handler.Warp (run)
import Server
import ServerMonad (ConfigMonad(..), ServerMonad(..))
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  config <- runConfigMonad getConfiguration
  printError $ show config
  flip run (app config) $ configPort config

getConfiguration :: ConfigMonad ServerConfiguration
getConfiguration = do
  configFilePath <- liftIO getArgs >>= pure . listToMaybe
  maybe (pure $ Right defaultConfiguration) readConfiguration configFilePath >>= pure . either error id

printError :: String -> IO ()
printError = hPutStrLn stderr
