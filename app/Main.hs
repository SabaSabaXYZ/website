module Main where

import Control.Monad ((<=<))
import Data.Maybe (fromMaybe, listToMaybe)
import Network.Wai.Handler.Warp (Port(..), run)
import Server
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = getPort >>= flip run app

getPort :: IO Port
getPort = getArgs >>= pure . fromMaybe 5000 . (readMaybe <=< listToMaybe)
