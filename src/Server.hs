module Server where

import Control.Monad.IO.Class (liftIO)
import CssContentType
import Lucid
import RenderBlog (renderBlog)
import Servant
import Servant.HTML.Lucid (HTML(..))
import qualified Data.Text as T
import qualified Data.Text.IO as T

app :: Application
app = serve apiProxy api

apiProxy :: Proxy Api
apiProxy = Proxy

type Api = MainPage :<|> BlogPost :<|> Themes
type MainPage = Get '[HTML] (Html ())
type BlogPost = "blog" :> QueryParam "id" BlogId :> Get '[HTML] (Html ())
type Themes = DarkTheme :<|> LightTheme
type DarkTheme = "dark" :> Get '[CSS] T.Text
type LightTheme = "light" :> Get '[CSS] T.Text

type BlogId = FilePath

api :: Server Api
api = mainPage :<|> blogPost :<|> themes

mainPage :: Handler (Html ())
mainPage = pure $ with doctypehtml_ [lang_ "en"] $ do
  head_ $ do
    title_ $ toHtml siteTitle
    meta_ [charset_ "utf8"]
    meta_ [name_ "description", content_ "width=device-width"]
    link_ [rel_ "stylesheet", href_ "/dark"]
  body_ $ div_ [role_ "main"] $ do
    h1_ $ toHtml siteTitle

blogPost :: Maybe BlogId -> Handler (Html ())
blogPost Nothing = mainPage
blogPost (Just blogId) = findBlogPost blogId >>= pure . renderBlog

findBlogPost :: BlogId -> Handler T.Text
findBlogPost = liftIO . T.readFile . (<>) "/static/"

themes :: Server Themes
themes = darkTheme :<|> lightTheme

-- TODO Modify this endpoint to take in a single hex colour value and produce a stylesheet from it.
darkTheme :: Handler T.Text
darkTheme = pure mempty

-- TODO Modify this endpoint to take in a single hex colour value and produce a stylesheet from it. The strategy used here should be the inverse of the one used by the dark path.
lightTheme :: Handler T.Text
lightTheme = pure mempty

htmlContainer :: Html a -> Html a
htmlContainer = id

siteTitle :: T.Text
siteTitle = "My Site"
