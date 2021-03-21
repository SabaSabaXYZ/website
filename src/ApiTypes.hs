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
type Themes = "style" :> (DarkTheme :<|> LightTheme)
type DarkTheme = "dark" :> QueryParam "red" Integer :> QueryParam "green" Integer :> QueryParam "blue" Integer :> Get '[CSS] C.Css
type LightTheme = "light" :> QueryParam "red" Integer :> QueryParam "green" Integer :> QueryParam "blue" Integer :> Get '[CSS] C.Css
type ThemeParam = QueryParam "theme" Theme

apiProxy :: Proxy Api
apiProxy = Proxy
