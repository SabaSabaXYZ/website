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

apiProxy :: Proxy Api
apiProxy = Proxy

api :: Server Api
api = page :<|> themes

page :: Server Page
page = mainPage :<|> blogPost

mainPage :: Maybe Theme -> Handler (Html ())
mainPage = flip blogPost "index"

blogPost :: Maybe Theme -> BlogId -> Handler (Html ())
blogPost theme = htmlContainer theme . renderBlog <=< findBlogPost

findBlogPost :: BlogId -> Handler T.Text
findBlogPost = liftIO . T.readFile . (<>) staticPath . flip (<>) (T.unpack markdownExtension)

themes :: Server Themes
themes = darkTheme :<|> lightTheme

darkTheme :: Maybe Integer -> Maybe Integer -> Maybe Integer -> Handler C.Css
darkTheme red green blue = pure $ darkStyle $ getColorFromInput Dark red green blue

lightTheme :: Maybe Integer -> Maybe Integer -> Maybe Integer -> Handler C.Css
lightTheme red green blue = pure $ lightStyle $ getColorFromInput Light red green blue

getColorFromInput :: LightDark -> Maybe Integer -> Maybe Integer -> Maybe Integer -> C.Color
getColorFromInput lightDark redColor greenColor blueColor = do
  let red = getIndividualColor lightDark redColor
  let green = getIndividualColor lightDark greenColor
  let blue = getIndividualColor lightDark blueColor
  C.rgba red green blue 1

getIndividualColor :: LightDark -> Maybe Integer -> Integer
getIndividualColor color = flip mod 0x100 . fromMaybe (defaultColor color)

defaultColor :: LightDark -> Integer
defaultColor Dark = 0x00
defaultColor Light = 0xFF
