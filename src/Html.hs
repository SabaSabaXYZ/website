module Html where

import ApiTypes
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..), liftIO)
import Data.Maybe (fromMaybe)
import Lucid
import Servant
import System.Directory (getDirectoryContents)
import qualified Data.Text as T
import qualified Data.Text.IO as T

htmlContainer :: (MonadIO m) => Maybe Theme -> Html a -> m (Html ())
htmlContainer theme contents = do
  nav <- navigation theme
  pure $ void $ with doctypehtml_ [lang_ "en"] $ do
    head_ $ do
      title_ $ toHtml siteTitle
      meta_ [charset_ "utf8"]
      meta_ [name_ "description", content_ "width=device-width"]
      link_ [rel_ "stylesheet", href_ $ getTheme theme]
    body_ $ do
      nav
      div_ [role_ "main"] contents

navigation :: (MonadIO m) => Maybe Theme -> m (Html ())
navigation theme = blogList theme >>= pure . div_ [role_ "navigation"] . ul_ [class_ "blog-links"]

blogList :: (MonadIO m) => Maybe Theme -> m (Html ())
blogList theme = liftIO $ getDirectoryContents staticPath >>= pure . foldMap (blogListItem theme) . filter (T.isSuffixOf markdownExtension) . fmap T.pack

blogListItem :: Maybe Theme -> T.Text -> Html ()
blogListItem theme path = do
  case blogLink path of
    Nothing -> pure $ mempty
    Just file -> li_ [class_ "blog-link"] $ a_ [href_ $ makeLink theme file] $ toHtml file

makeLink :: Maybe Theme -> T.Text -> T.Text
makeLink Nothing link = link
makeLink (Just theme) link = link <> "?theme=" <> toQueryParam theme

blogLink :: T.Text -> Maybe T.Text
blogLink = T.stripSuffix markdownExtension

siteTitle :: T.Text
siteTitle = "My Site"

staticPath :: FilePath
staticPath = "static/"

markdownExtension :: T.Text
markdownExtension = ".md"

getTheme :: Maybe Theme -> T.Text
getTheme Nothing = "/style/dark"
getTheme (Just theme)
  | themeType theme == Dark = "/style/dark?red=" <> toQueryParam (themeRed theme) <> "&green=" <> toQueryParam (themeGreen theme) <> "&blue=" <> toQueryParam (themeBlue theme)
  | themeType theme == Light = "/style/light?red=" <> toQueryParam (themeRed theme) <> "&green=" <> toQueryParam (themeGreen theme) <> "&blue=" <> toQueryParam (themeBlue theme)
