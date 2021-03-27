module ApiTypes where

import Control.Monad ((<=<))
import CssContentType
import Lucid
import Servant
import Servant.HTML.Lucid (HTML(..))
import qualified Clay as C
import qualified Data.Text as T

type Api = Styling :<|> Page
type Page = MainPage :<|> BlogPost
type MainPage = ThemeParam :> Get '[HTML] (Html ())
type BlogPost = ThemeParam :> Capture "id" BlogId :> Get '[HTML] (Html ())
type Styling = "style" :> ThemeParam :> Get '[CSS] C.Css
type ThemeParam = QueryParam "theme" Theme

type BlogId = FilePath

data LightDark = Light | Dark deriving (Eq)

instance FromHttpApiData LightDark where
  parseQueryParam "dark" = Right Dark
  parseQueryParam "light" = Right Light
  parseQueryParam x = Left $ "Invalid value " <> x <> ". Value must be either 'dark' or 'light'."

instance ToHttpApiData LightDark where
  toQueryParam Dark = "dark"
  toQueryParam Light = "light"

data Theme = Theme { themeType :: !LightDark
                   , themeRed :: !Integer
                   , themeGreen :: !Integer
                   , themeBlue :: !Integer
                   }

instance FromHttpApiData Theme where
  parseQueryParam (T.splitOn "," -> [lightText, redText, greenText, blueText]) = do
    let parseColorComponent = pure . flip mod 0x100 <=< parseQueryParam
    light <- parseQueryParam lightText
    red <- parseColorComponent redText
    green <- parseColorComponent greenText
    blue <- parseColorComponent blueText
    pure $ Theme { themeType = light, themeRed = red, themeGreen = green, themeBlue = blue }
  parseQueryParam theme = Left $ "Invalid value '" <> theme <> "'. Value must contain four values delimited by commas."

instance ToHttpApiData Theme where
  toQueryParam theme = toQueryParam (themeType theme) <> "," <> toQueryParam (themeRed theme) <> "," <> toQueryParam (themeGreen theme) <> "," <> toQueryParam (themeBlue theme)

apiProxy :: Proxy Api
apiProxy = Proxy

safeBlogLink :: Maybe Theme -> BlogId -> T.Text
safeBlogLink theme blogId = toUrlPiece $ safeLink apiProxy (Proxy @BlogPost) theme blogId

safeStylingLink :: Maybe Theme -> T.Text
safeStylingLink theme = toUrlPiece $ safeLink apiProxy (Proxy @Styling) theme
