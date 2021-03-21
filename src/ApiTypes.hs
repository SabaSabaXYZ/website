module ApiTypes where

import CssContentType
import Html
import Lucid
import Servant
import Servant.HTML.Lucid (HTML(..))
import qualified Clay as C

type Api = Page :<|> Themes
type Page = MainPage :<|> BlogPost
type MainPage = ThemeParam :> Get '[HTML] (Html ())
type BlogPost = ThemeParam :> Capture "id" BlogId :> Get '[HTML] (Html ())
type Themes = DarkTheme :<|> LightTheme
type DarkTheme = "style" :> "dark" :> QueryParam "red" Integer :> QueryParam "green" Integer :> QueryParam "blue" Integer :> Get '[CSS] C.Css
type LightTheme = "style" :> "light" :> QueryParam "red" Integer :> QueryParam "green" Integer :> QueryParam "blue" Integer :> Get '[CSS] C.Css
type ThemeParam = QueryParam "theme" Theme

apiProxy :: Proxy Api
apiProxy = Proxy

blogProxy :: Proxy BlogPost
blogProxy = Proxy

darkThemeProxy :: Proxy DarkTheme
darkThemeProxy = Proxy

lightThemeProxy :: Proxy LightTheme
lightThemeProxy = Proxy

safeBlogLink :: MkLink BlogPost Link
safeBlogLink = safeLink apiProxy blogProxy

safeDarkThemeLink :: MkLink DarkTheme Link
safeDarkThemeLink = safeLink apiProxy darkThemeProxy

safeLightThemeLink :: MkLink LightTheme Link
safeLightThemeLink = safeLink apiProxy lightThemeProxy
