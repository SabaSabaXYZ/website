module Server where

import Control.Monad ((<=<), void)
import Control.Monad.IO.Class (liftIO)
import CssContentType
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
type MainPage = Get '[HTML] (Html ())
type BlogPost = "blog" :> Capture "id" BlogId :> Get '[HTML] (Html ())
type Themes = DarkTheme :<|> LightTheme
type DarkTheme = "dark" :> Get '[CSS] C.Css
type LightTheme = "light" :> Get '[CSS] C.Css

type BlogId = FilePath

api :: Server Api
api = page :<|> themes

page :: Server Page
page = mainPage :<|> blogPost

mainPage :: Handler (Html ())
mainPage = htmlContainer $ h1_ $ toHtml siteTitle

blogPost :: BlogId -> Handler (Html ())
blogPost = htmlContainer . renderBlog <=< findBlogPost

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

htmlContainer :: Html a -> Handler (Html ())
htmlContainer contents = do
  nav <- navigation
  pure $ void $ with doctypehtml_ [lang_ "en"] $ do
    head_ $ do
      title_ $ toHtml siteTitle
      meta_ [charset_ "utf8"]
      meta_ [name_ "description", content_ "width=device-width"]
      link_ [rel_ "stylesheet", href_ "/dark"]
    body_ $ do
      nav
      div_ [role_ "main"] contents

navigation :: Handler (Html ())
navigation = liftIO blogList >>= pure . div_ [role_ "navigation"] . ul_ [class_ "blog-links"]

blogList :: IO (Html ())
blogList = getDirectoryContents staticPath >>= pure . foldMap blogListItem . filter (T.isSuffixOf markdownExtension) . fmap T.pack

blogListItem :: T.Text -> Html ()
blogListItem path = do
  case blogLink path of
    Nothing -> pure $ mempty
    Just file -> li_ [class_ "blog-link"] $ a_ [href_ file] $ toHtml file

blogLink :: T.Text -> Maybe T.Text
blogLink = pure . (<>) "/blog/" <=< T.stripSuffix markdownExtension

siteTitle :: T.Text
siteTitle = "My Site"

staticPath :: FilePath
staticPath = "static/"

markdownExtension :: T.Text
markdownExtension = ".md"
