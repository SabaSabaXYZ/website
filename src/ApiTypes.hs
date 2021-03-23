module ApiTypes where

import CssContentType
import Lucid
import Servant
import Servant.HTML.Lucid (HTML(..))
import qualified Clay as C
import qualified Data.Text as T

type Api = Page :<|> Themes
type Page = MainPage :<|> BlogPost
type MainPage = ThemeParam :> Get '[HTML] (Html ())
type BlogPost = ThemeParam :> Capture "id" BlogId :> Get '[HTML] (Html ())
type Themes = DarkTheme :<|> LightTheme
type DarkTheme = "style" :> "dark" :> QueryParam "red" Integer :> QueryParam "green" Integer :> QueryParam "blue" Integer :> Get '[CSS] C.Css
type LightTheme = "style" :> "light" :> QueryParam "red" Integer :> QueryParam "green" Integer :> QueryParam "blue" Integer :> Get '[CSS] C.Css
type ThemeParam = QueryParam "theme" Theme

type BlogId = FilePath

data LightDark = Light | Dark deriving (Eq)

instance FromHttpApiData LightDark where
  parseQueryParam "0" = Right Dark
  parseQueryParam "1" = Right Light
  parseQueryParam x = Left $ "Invalid value " <> x <> ". Value must be either '0' or '1'."

instance ToHttpApiData LightDark where
  toQueryParam Dark = "0"
  toQueryParam Light = "1"

data Theme = Theme { themeType :: !LightDark
                   , themeRed :: !Integer
                   , themeGreen :: !Integer
                   , themeBlue :: !Integer
                   }

instance FromHttpApiData Theme where
  parseQueryParam theme = do
    case T.splitOn "," theme of
      [lightText, redText, greenText, blueText] -> do
        light <- parseQueryParam lightText
        red <- parseQueryParam redText
        green <- parseQueryParam greenText
        blue <- parseQueryParam blueText
        pure $ Theme { themeType = light, themeRed = red, themeGreen = green, themeBlue = blue }
      _ -> Left $ "Invalid value '" <> theme <> "'. Value must contain four integer values delimited by commas."

instance ToHttpApiData Theme where
  toQueryParam theme = toQueryParam (themeType theme) <> "," <> toQueryParam (themeRed theme) <> "," <> toQueryParam (themeGreen theme) <> "," <> toQueryParam (themeBlue theme)

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
