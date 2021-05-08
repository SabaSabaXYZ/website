module ServerMonad where

import Configuration (ServerConfiguration(..), MonadReadConfig(..))
import Control.Monad.IO.Class (MonadIO(..), liftIO)
import Control.Monad.Reader (MonadReader(..), ReaderT(..))
import qualified Data.ByteString as B

newtype ConfigMonad a = ConfigMonad { runConfigMonad :: IO a
                                    } deriving (Functor, Applicative, Monad, MonadIO)

newtype ServerMonad a = ServerMonad { runServerMonad :: ReaderT ServerConfiguration IO a
                                    } deriving (Functor, Applicative, Monad, MonadIO, MonadReader ServerConfiguration)

instance MonadReadConfig ConfigMonad where
  readConfigFile = liftIO . B.readFile
