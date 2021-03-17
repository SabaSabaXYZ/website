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
type DarkTheme = "dark" :> Get '[CSS] C.Css
type LightTheme = "light" :> Get '[CSS] C.Css
type ThemeParam = QueryParam "light" Bool

type BlogId = FilePath
-- TODO Use a ReaderT monad to insert this value into functions instead of passing it in explicitly.
type UseLightTheme = Maybe Bool

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

-- TODO Modify this endpoint to take in a single hex colour value and produce a stylesheet from it.
darkTheme :: Handler C.Css
darkTheme = pure $ darkStyle C.saddlebrown

-- TODO Modify this endpoint to take in a single hex colour value and produce a stylesheet from it. The strategy used here should be the inverse of the one used by the dark path.
lightTheme :: Handler C.Css
lightTheme = pure $ lightStyle C.blanchedalmond

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
