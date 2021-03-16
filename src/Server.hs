module Server where

import CssContentType
import Servant
import Lucid
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

type BlogId = Integer

api :: Server Api
api = mainPage :<|> blogPost :<|> themes

mainPage :: Handler (Html ())
mainPage = undefined

blogPost :: Maybe BlogId -> Handler (Html ())
blogPost blogId = undefined

themes :: Server Themes
themes = darkTheme :<|> lightTheme

darkTheme :: Handler T.Text
darkTheme = undefined

lightTheme :: Handler T.Text
lightTheme = undefined
