module CssContentType where

import Data.Text.Lazy.Encoding (encodeUtf8)
import Network.HTTP.Media ((//), (/:))
import Servant
import qualified Clay as C

data CSS

instance Accept CSS where
  contentType _ = "text" // "css" /: ("charset", "utf-8")

instance MimeRender CSS (C.Css) where
  mimeRender _ val = encodeUtf8 $ C.renderWith C.compact [] val
