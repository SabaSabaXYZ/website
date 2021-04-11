module Html where

import ApiTypes
import Control.Exception.Safe (SomeException)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..), liftIO)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Lucid
import Sanitize
import Servant
import System.Directory (getDirectoryContents)
import qualified Data.Text as T
import qualified Data.Text.IO as T

htmlContainer :: (MonadIO m) => Maybe Theme -> Maybe BlogId -> Html a -> m (Html ())
htmlContainer theme maybeBlogId contents = do
  nav <- navigation theme
  themeConfig <- themeConfiguration theme maybeBlogId
  pure $ sanitizeHtml $ void $ with doctypehtml_ [lang_ "en"] $ do
    head_ $ do
      title_ $ toHtml siteTitle
      meta_ [charset_ "utf8"]
      meta_ [name_ "description", content_ "width=device-width"]
      link_ [rel_ "stylesheet", href_ $ safeStylingLink theme]
    body_ $ do
      div_ [role_ "main"] contents
      nav
      themeConfig

themeConfiguration :: (MonadIO m) => Maybe Theme -> Maybe BlogId -> m (Html ())
themeConfiguration (fromMaybe defaultTheme -> theme) (fromMaybe defaultBlogId -> blogId) = pure $ do
  div_ [role_ "banner"] $ do
    h2_ "Theme:"
    form_ [action_ $ safeChangeThemeLink blogId, method_ "POST"] $ do
      lightDarkInput (themeType theme)
      colorInput (themeRed theme) "Red"
      colorInput (themeGreen theme) "Green"
      colorInput (themeBlue theme) "Blue"
      div_ [class_ "input"] $ input_ [type_ "submit"]

lightDarkInput :: LightDark -> Html ()
lightDarkInput lightDark = do
  let fieldId = "themeType"
  let useDark = lightDark == Dark
  div_ [class_ "input"] $ do
    label_ [for_ fieldId] $ toHtml @T.Text "Style"
    select_ [id_ fieldId, name_ fieldId] $ do
      option_ (attributes "dark" useDark) $ toHtml @T.Text "Dark"
      option_ (attributes "light" $ not useDark) $ toHtml @T.Text "Light"
  where
    attributes value isSelected = if isSelected then [value_ value, selected_ mempty] else [value_ value]

colorInput :: Integer -> T.Text -> Html ()
colorInput value label = let fieldId = "theme" <> label in div_ [class_ "input"] $ do
  label_ [for_ fieldId] $ toHtml label
  input_ [id_ fieldId, name_ fieldId, value_ $ T.pack $ show value, type_ "number", min_ "0", max_ "255", step_ "1"]

navigation :: (MonadIO m) => Maybe Theme -> m (Html ())
navigation theme = do
  blogListItems <- blogList theme >>= pure . ul_ [class_ "blog-links"]
  pure $ div_ [role_ "navigation"] $ do
    h2_ "Articles:"
    blogListItems

blogList :: (MonadIO m) => Maybe Theme -> m (Html ())
blogList theme = liftIO $ getDirectoryContents staticPath >>= pure . foldMap (blogListItem theme) . sort . filter (T.isSuffixOf markdownExtension) . fmap T.pack

blogListItem :: Maybe Theme -> T.Text -> Html ()
blogListItem theme (blogLink -> Nothing) = pure $ mempty
blogListItem theme (blogLink -> (Just file)) = li_ [class_ "blog-link"] $ a_ [href_ $ safeBlogLink theme $ T.unpack file] $ toHtml file

blogLink :: T.Text -> Maybe T.Text
blogLink = T.stripSuffix markdownExtension

blogNotFound :: (MonadIO m) => Maybe Theme -> BlogId -> SomeException -> m (Html ())
blogNotFound theme blogId _ = htmlContainer theme Nothing $ do
  div_ [class_ "not-found"] $ do
    h1_ $ toHtml @T.Text "Blog not found"
    p_ $ toHtml $ "Blog post " <> T.pack blogId <> " could not found."

siteTitle :: T.Text
siteTitle = "My Site"

staticPath :: FilePath
staticPath = "static/"

markdownExtension :: T.Text
markdownExtension = ".md"
