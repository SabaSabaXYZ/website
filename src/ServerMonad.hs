module ServerMonad where

import Configuration (ServerConfiguration(..), MonadReadConfig(..))
import Control.Exception.Safe (MonadCatch(..), MonadThrow(..))
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Except (ExceptT(..))
import Control.Monad.IO.Class (MonadIO(..), liftIO)
import Control.Monad.Reader (MonadReader(..), ReaderT(..))
import Servant (ServerError)
import qualified Data.ByteString as B

newtype ConfigMonad a = ConfigMonad { runConfigMonad :: IO a
                                    } deriving (Functor, Applicative, Monad, MonadIO)

newtype ServerMonad a = ServerMonad { runServerMonad :: ReaderT ServerConfiguration (ExceptT ServerError IO) a
                                    } deriving (Functor, Applicative, Monad, MonadIO, MonadReader ServerConfiguration, MonadError ServerError, MonadThrow, MonadCatch)

instance MonadReadConfig ConfigMonad where
  readConfigFile = liftIO . B.readFile
