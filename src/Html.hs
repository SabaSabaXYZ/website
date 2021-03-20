module Html where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..), liftIO)
import Data.Maybe (fromMaybe)
import Lucid
import Servant
import System.Directory (getDirectoryContents)
import qualified Data.Text as T
import qualified Data.Text.IO as T

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

type UseLightTheme = Maybe Bool
type BlogId = FilePath

htmlContainer :: (MonadIO m) => UseLightTheme -> Html a -> m (Html ())
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

navigation :: (MonadIO m) => UseLightTheme -> m (Html ())
navigation useLight = blogList useLight >>= pure . div_ [role_ "navigation"] . ul_ [class_ "blog-links"]

blogList :: (MonadIO m) => UseLightTheme -> m (Html ())
blogList useLight = liftIO $ getDirectoryContents staticPath >>= pure . foldMap (blogListItem useLight) . filter (T.isSuffixOf markdownExtension) . fmap T.pack

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
