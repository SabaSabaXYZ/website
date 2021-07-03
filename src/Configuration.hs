module Configuration where

import Control.Monad ((<=<))
import Data.Aeson (FromJSON(..), eitherDecodeStrict')
import GHC.Generics (Generic(..))
import Network.Wai.Handler.Warp (Port(..))
import qualified Data.ByteString as B
import qualified Data.Text as T

class (Monad m) => MonadReadConfig m where
  readConfigFile :: FilePath -> m B.ByteString

data ServerConfiguration = ServerConfiguration { configPort :: !Port
                                               , configShowExceptions :: !Bool
                                               , configTitle :: !T.Text
                                               } deriving (Show, Generic)

instance FromJSON ServerConfiguration

defaultConfiguration :: ServerConfiguration
defaultConfiguration = ServerConfiguration { configPort = 5000
                                           , configShowExceptions = False
                                           , configTitle = "Default Title"
                                           }

readConfiguration :: (MonadReadConfig m) => FilePath -> m (Either String ServerConfiguration)
readConfiguration = pure . eitherDecodeStrict' <=< readConfigFile
