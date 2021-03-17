module Server where

import Control.Monad ((<=<), void)
import Control.Monad.IO.Class (liftIO)
import CssContentType
import Data.Maybe (fromMaybe)
import Lucid
import RenderBlog (renderBlog)
import Servant
import Servant.HTML.Lucid (HTML(..))
import StyleSheet
import System.Directory (getCurrentDirectory, getDirectoryContents)
import qualified Clay as C
import qualified Data.Text as T
import qualified Data.Text.IO as T

app :: Application
app = serve apiProxy api

apiProxy :: Proxy Api
apiProxy = Proxy

type Api = Page :<|> Themes
type Page = MainPage :<|> BlogPost
type MainPage = ThemeParam :> Get '[HTML] (Html ())
type BlogPost = ThemeParam :> Capture "id" BlogId :> Get '[HTML] (Html ())
type Themes = "style" :> (DarkTheme :<|> LightTheme)
type DarkTheme = "dark" :> QueryParam "red" Integer :> QueryParam "green" Integer :> QueryParam "blue" Integer :> Get '[CSS] C.Css
type LightTheme = "light" :> QueryParam "red" Integer :> QueryParam "green" Integer :> QueryParam "blue" Integer :> Get '[CSS] C.Css
type ThemeParam = QueryParam "light" Bool

type BlogId = FilePath
type UseLightTheme = Maybe Bool
data LightDark = Light | Dark deriving (Eq)

api :: Server Api
api = page :<|> themes

page :: Server Page
page = mainPage :<|> blogPost

mainPage :: UseLightTheme -> Handler (Html ())
mainPage = flip blogPost "index"

blogPost :: UseLightTheme -> BlogId -> Handler (Html ())
blogPost useLight = htmlContainer useLight . renderBlog <=< findBlogPost

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
getIndividualColor Dark value = flip mod 0x100 $ fromMaybe 0x00 value
getIndividualColor Light value = flip mod 0x100 $ fromMaybe 0xFF value

htmlContainer :: UseLightTheme -> Html a -> Handler (Html ())
htmlContainer useLight contents = do
  nav <- navigation useLight
  pure $ void $ with doctypehtml_ [lang_ "en"] $ do
    head_ $ do
      title_ $ toHtml siteTitle
      meta_ [charset_ "utf8"]
      meta_ [name_ "description", content_ "width=device-width"]
      link_ [rel_ "stylesheet", href_ $ getTheme useLight]
    body_ $ do
      nav
      div_ [role_ "main"] contents

navigation :: UseLightTheme -> Handler (Html ())
navigation useLight = liftIO (blogList useLight) >>= pure . div_ [role_ "navigation"] . ul_ [class_ "blog-links"]

blogList :: UseLightTheme -> IO (Html ())
blogList useLight = getDirectoryContents staticPath >>= pure . foldMap (blogListItem useLight) . filter (T.isSuffixOf markdownExtension) . fmap T.pack

blogListItem :: UseLightTheme -> T.Text -> Html ()
blogListItem useLight path = do
  case blogLink path of
    Nothing -> pure $ mempty
    Just file -> li_ [class_ "blog-link"] $ a_ [href_ $ makeLink useLight file] $ toHtml file

makeLink :: UseLightTheme -> T.Text -> T.Text
makeLink useLight link = let lightThemeOn = useLightTheme useLight in if lightThemeOn then link <> "?light=true" else link <> "?light=false"

blogLink :: T.Text -> Maybe T.Text
blogLink = T.stripSuffix markdownExtension

siteTitle :: T.Text
siteTitle = "My Site"

staticPath :: FilePath
staticPath = "static/"

markdownExtension :: T.Text
markdownExtension = ".md"

useLightTheme :: UseLightTheme -> Bool
useLightTheme = fromMaybe False

getTheme :: UseLightTheme -> T.Text
getTheme theme = let lightThemeOn = useLightTheme theme in if lightThemeOn then "/style/light" else "/style/dark"
