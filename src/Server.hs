module Server where

import ApiTypes
import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
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
styling Nothing = pure $ darkStyle $ getColorFromInput 0 0 0
styling (Just theme) = pure $ getStyleFromTheme (themeType theme) $ getColorFromInput (themeRed theme) (themeGreen theme) (themeBlue theme)

getStyleFromTheme :: LightDark -> C.Color -> C.Css
getStyleFromTheme Dark = darkStyle
getStyleFromTheme Light = lightStyle

getColorFromInput :: Integer -> Integer -> Integer -> C.Color
getColorFromInput redColor greenColor blueColor = do
  let red = getIndividualColor redColor
  let green = getIndividualColor greenColor
  let blue = getIndividualColor blueColor
  C.rgba red green blue 1

getIndividualColor :: Integer -> Integer
getIndividualColor = flip mod 0x100
