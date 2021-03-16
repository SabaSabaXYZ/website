module Server where

import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import CssContentType
import Lucid
import RenderBlog (renderBlog)
import Servant
import Servant.HTML.Lucid (HTML(..))
import System.Directory (getCurrentDirectory, getDirectoryContents)
import qualified Data.Text as T
import qualified Data.Text.IO as T

app :: Application
app = serve apiProxy api

apiProxy :: Proxy Api
apiProxy = Proxy

type Api = MainPage :<|> BlogPost :<|> Themes :<|> TestPage
type MainPage = Get '[HTML] (Html ())
type BlogPost = "blog" :> Capture "id" BlogId :> Get '[HTML] (Html ())
type Themes = DarkTheme :<|> LightTheme
type DarkTheme = "dark" :> Get '[CSS] T.Text
type LightTheme = "light" :> Get '[CSS] T.Text
type TestPage = "test" :> Get '[HTML] T.Text

type BlogId = FilePath

api :: Server Api
api = mainPage :<|> blogPost :<|> themes :<|> testPage

mainPage :: Handler (Html ())
mainPage = htmlContainer $ h1_ $ toHtml siteTitle

blogPost :: BlogId -> Handler (Html ())
blogPost = htmlContainer . renderBlog <=< findBlogPost

findBlogPost :: BlogId -> Handler T.Text
findBlogPost = liftIO . T.readFile . (<>) staticPath . flip (<>) ".md"

themes :: Server Themes
themes = darkTheme :<|> lightTheme

-- TODO Modify this endpoint to take in a single hex colour value and produce a stylesheet from it.
darkTheme :: Handler T.Text
darkTheme = pure mempty

-- TODO Modify this endpoint to take in a single hex colour value and produce a stylesheet from it. The strategy used here should be the inverse of the one used by the dark path.
lightTheme :: Handler T.Text
lightTheme = pure mempty

htmlContainer :: Html a -> Handler (Html ())
htmlContainer contents = do
  footer <- htmlFooter
  pure $ with doctypehtml_ [lang_ "en"] $ do
    head_ $ do
      title_ $ toHtml siteTitle
      meta_ [charset_ "utf8"]
      meta_ [name_ "description", content_ "width=device-width"]
      link_ [rel_ "stylesheet", href_ "/dark"]
    body_ $ div_ [role_ "main"] $ do
      contents
      footer

htmlFooter :: Handler (Html ())
htmlFooter = liftIO blogLinks >>= pure . div_ [role_ "footer"] . ul_ [class_ "blog-links"]

blogLinks :: IO (Html ())
blogLinks = getDirectoryContents staticPath >>= pure . foldMap blogLink

blogLink :: FilePath -> Html ()
blogLink = li_ [class_ "blog-link"] . toHtml

siteTitle :: T.Text
siteTitle = "My Site"

testPage :: Handler T.Text
testPage = liftIO getCurrentDirectory >>= pure . T.pack

staticPath :: FilePath
staticPath = "static/"
