module Html where

import ApiTypes
import Configuration (ServerConfiguration(..))
import Control.Exception.Safe (SomeException)
import Control.Monad (void)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Reader (MonadReader(..))
import Data.Bifunctor (first)
import Data.ByteString.Lazy (ByteString(..))
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime(..))
import Data.Time.Format (formatTime, defaultTimeLocale, iso8601DateFormat)
import Lucid
import Sanitize
import Servant
import ServerMonad (MonadDirectory(..))
import System.Directory (getDirectoryContents, getModificationTime)
import System.FilePath.Posix ((</>))
import qualified Data.Text as T
import qualified Data.Text.IO as T

htmlContainer :: (MonadDirectory m, MonadReader ServerConfiguration m) => Maybe Theme -> Maybe BlogId -> Html a -> m (Html ())
htmlContainer theme maybeBlogId contents = do
  nav <- navigation theme
  themeConfig <- themeConfiguration theme maybeBlogId
  title <- siteTitle
  pure $ sanitizeHtml $ void $ with doctypehtml_ [lang_ "en"] $ do
    head_ $ do
      title_ $ toHtml title
      meta_ [charset_ "utf8"]
      meta_ [name_ "description", content_ "A personal website with custom theming"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
      link_ [rel_ "stylesheet", href_ $ safeStylingLink theme]
    body_ $ do
      div_ [role_ "main"] contents
      nav
      themeConfig

themeConfiguration :: (MonadDirectory m) => Maybe Theme -> Maybe BlogId -> m (Html ())
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

navigation :: (MonadDirectory m) => Maybe Theme -> m (Html ())
navigation theme = do
  blogListItems <- blogList theme >>= pure . ul_ [class_ "blog-links"]
  pure $ div_ [role_ "navigation"] $ do
    h2_ "Articles:"
    blogListItems

blogList :: (MonadDirectory m) => Maybe Theme -> m (Html ())
blogList theme = readDirectory staticPath >>= mapM blogModificationTime . fmap T.unpack . filter (T.isSuffixOf markdownExtension) >>= pure . foldMap (blogListItem theme) . sortOn snd . fmap (first T.pack)

blogModificationTime :: (MonadDirectory m) => FilePath -> m (FilePath, UTCTime)
blogModificationTime filePath = readModificationTime (staticPath </> filePath) >>= pure . (,) filePath

blogListItem :: Maybe Theme -> (T.Text, UTCTime) -> Html ()
blogListItem theme (first blogLink -> (Nothing, _)) = pure $ mempty
blogListItem theme (first blogLink -> (Just file, time)) = li_ [class_ "blog-link"] $ a_ [href_ $ safeBlogLink theme $ T.unpack file] $ toHtml $ blogNameWithDate file time

blogNameWithDate :: T.Text -> UTCTime -> T.Text
blogNameWithDate file time = file <> " (last modified: " <> T.pack (formatTime defaultTimeLocale (iso8601DateFormat Nothing) $ utctDay time) <> ")"

blogLink :: T.Text -> Maybe T.Text
blogLink = T.stripSuffix markdownExtension

imageNotFound :: (MonadError ServerError m) => SomeException -> m a
imageNotFound _ = throwError $ err404 { errBody = "No image found." }

blogNotFound :: (MonadDirectory m, MonadError ServerError m, MonadReader ServerConfiguration m) => Maybe Theme -> BlogId -> SomeException -> m a
blogNotFound theme blogId exceptionReason = do
  showExceptions <- ask >>= pure . configShowExceptions
  body <- htmlContainer theme Nothing $ do
    div_ [class_ "not-found"] $ do
      h1_ $ toHtml @T.Text "Blog not found"
      p_ $ do
        toHtml @T.Text "Blog post "
        em_ $ toHtml $ T.pack blogId
        toHtml @T.Text " could not found."
      if showExceptions then p_ $ toHtml $ T.pack $ show exceptionReason else pure ()
  throwError $ err404 { errBody = renderBS body }

siteTitle :: (MonadReader ServerConfiguration m) => m T.Text
siteTitle = ask >>= pure . configTitle

staticPath :: FilePath
staticPath = "static"

imagePath :: FilePath
imagePath = staticPath </> "img"

markdownExtension :: T.Text
markdownExtension = ".md"
