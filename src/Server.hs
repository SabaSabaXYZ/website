module Server where

import ApiTypes
import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import Html
import Lucid
import RenderBlog (renderBlog)
import Servant
import StyleSheet
import qualified Clay as C
import qualified Data.Text as T
import qualified Data.Text.IO as T

app :: Application
app = serve apiProxy api

api :: Server Api
api = styling :<|> page

page :: Server Page
page = mainPage :<|> blogPost

mainPage :: Maybe Theme -> Handler (Html ())
mainPage = flip blogPost "index"

blogPost :: Maybe Theme -> BlogId -> Handler (Html ())
blogPost theme = htmlContainer theme . renderBlog <=< findBlogPost

findBlogPost :: BlogId -> Handler T.Text
findBlogPost = liftIO . T.readFile . (<>) staticPath . flip (<>) (T.unpack markdownExtension)

styling :: Maybe Theme -> Handler C.Css
styling Nothing = pure $ darkStyle C.black
styling (Just theme) = pure $ getStyleFromTheme (themeType theme) $ C.rgba (themeRed theme) (themeGreen theme) (themeBlue theme) 1

getStyleFromTheme :: LightDark -> C.Color -> C.Css
getStyleFromTheme Dark = darkStyle
getStyleFromTheme Light = lightStyle
