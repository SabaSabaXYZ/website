module Server where

import ApiTypes
import Configuration (ServerConfiguration(..))
import Control.Exception.Safe (handleAny)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Data.Maybe (fromMaybe)
import Html
import Lucid
import RenderBlog (renderBlog)
import Servant
import ServerMonad (ServerMonad(..))
import StyleSheet
import System.FilePath.Posix ((</>))
import qualified Clay as C
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Text.IO.Utf8 as T

app :: ServerConfiguration -> Application
app config = serve apiProxy $ hoistServer apiProxy (serverMonadToHandler config) api

serverMonadToHandler :: ServerConfiguration -> ServerMonad a -> Handler a
serverMonadToHandler config = Handler . flip runReaderT config . runServerMonad

api :: ServerT Api ServerMonad
api = styling :<|> page

page :: ServerT Page ServerMonad
page = changeTheme :<|> imageLink :<|> mainPage :<|> blogPost

mainPage :: Maybe Theme -> ServerMonad (Html ())
mainPage = flip blogPost defaultBlogId

blogPost :: Maybe Theme -> BlogId -> ServerMonad (Html ())
blogPost theme blogId = handleAny (blogNotFound theme blogId) $ findBlogPost blogId >>= htmlContainer theme (Just blogId) . renderBlog

findBlogPost :: BlogId -> ServerMonad T.Text
findBlogPost = liftIO . T.readFile . (</>) staticPath . flip (<>) (T.unpack markdownExtension)

changeTheme :: Theme -> BlogId -> ServerMonad (Html ())
changeTheme theme = blogPost (Just theme)

imageLink :: ImageId -> ServerMonad B.ByteString
imageLink imageId = handleAny imageNotFound $ liftIO $ B.readFile $ imagePath </> imageId

styling :: Maybe Theme -> ServerMonad C.Css
styling (fromMaybe defaultTheme -> theme) = pure $ getStyleFromTheme (themeType theme) $ C.rgba (themeRed theme) (themeGreen theme) (themeBlue theme) 1

getStyleFromTheme :: LightDark -> C.Color -> C.Css
getStyleFromTheme Dark = darkStyle
getStyleFromTheme Light = lightStyle
