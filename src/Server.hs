module Server where

import ApiTypes
import Control.Exception.Safe (handleAny)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Html
import Lucid
import RenderBlog (renderBlog)
import Servant
import StyleSheet
import System.FilePath.Posix ((</>))
import qualified Clay as C
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Text.IO.Utf8 as T

app :: Application
app = serve apiProxy api

api :: Server Api
api = styling :<|> page

page :: Server Page
page = changeTheme :<|> imageLink :<|> mainPage :<|> blogPost

mainPage :: Maybe Theme -> Handler (Html ())
mainPage = flip blogPost defaultBlogId

blogPost :: Maybe Theme -> BlogId -> Handler (Html ())
blogPost theme blogId = handleAny (blogNotFound theme blogId) $ findBlogPost blogId >>= htmlContainer theme (Just blogId) . renderBlog

findBlogPost :: BlogId -> Handler T.Text
findBlogPost = liftIO . T.readFile . (</>) staticPath . flip (<>) (T.unpack markdownExtension)

changeTheme :: Theme -> BlogId -> Handler (Html ())
changeTheme theme = blogPost (Just theme)

imageLink :: ImageId -> Handler B.ByteString
imageLink imageId = handleAny imageNotFound $ liftIO $ B.readFile $ imagePath </> imageId

styling :: Maybe Theme -> Handler C.Css
styling (fromMaybe defaultTheme -> theme) = pure $ getStyleFromTheme (themeType theme) $ C.rgba (themeRed theme) (themeGreen theme) (themeBlue theme) 1

getStyleFromTheme :: LightDark -> C.Color -> C.Css
getStyleFromTheme Dark = darkStyle
getStyleFromTheme Light = lightStyle
