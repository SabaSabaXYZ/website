module ServerMonad where

import Configuration (ServerConfiguration(..), MonadReadConfig(..))
import Control.Exception.Safe (MonadCatch(..), MonadThrow(..))
import Control.Monad ((<=<))
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Except (ExceptT(..))
import Control.Monad.IO.Class (MonadIO(..), liftIO)
import Control.Monad.Reader (MonadReader(..), ReaderT(..))
import Data.Time.Clock (UTCTime(..))
import Servant (ServerError)
import System.Directory (getDirectoryContents, getModificationTime)
import qualified Data.ByteString as B
import qualified Data.Text as T

newtype ConfigMonad a = ConfigMonad { runConfigMonad :: IO a
                                    } deriving (Functor, Applicative, Monad, MonadIO)

newtype ServerMonad a = ServerMonad { runServerMonad :: ReaderT ServerConfiguration (ExceptT ServerError IO) a
                                    } deriving (Functor, Applicative, Monad, MonadIO, MonadReader ServerConfiguration, MonadError ServerError, MonadThrow, MonadCatch)

class (Monad m) => MonadDirectory m where
  readDirectory :: FilePath -> m [T.Text]
  readModificationTime :: FilePath -> m UTCTime

instance MonadReadConfig ConfigMonad where
  readConfigFile = liftIO . B.readFile

instance MonadDirectory ServerMonad where
  readDirectory = liftIO . (pure . fmap T.pack <=< getDirectoryContents)
  readModificationTime = liftIO . getModificationTime
