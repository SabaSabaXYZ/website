module Server where

import CssContentType
import Lucid
import RenderBlog (renderBlog)
import Servant
import Servant.HTML.Lucid (HTML(..))
import qualified Data.Text as T

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
  body_ $ do
    h1_ $ toHtml siteTitle

blogPost :: Maybe BlogId -> Handler (Html ())
blogPost Nothing = mainPage
blogPost (Just blogId) = mainPage

themes :: Server Themes
themes = darkTheme :<|> lightTheme

darkTheme :: Handler T.Text
darkTheme = pure mempty

lightTheme :: Handler T.Text
lightTheme = pure mempty

htmlContainer :: Html a -> Html a
htmlContainer = id

siteTitle :: T.Text
siteTitle = "My Site"
