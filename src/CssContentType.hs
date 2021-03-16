module CssContentType where

import Data.Text.Lazy (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Network.HTTP.Media ((//), (/:))
import Servant

data CSS

instance Accept CSS where
  contentType _ = "text" // "css" /: ("charset", "utf-8")

instance (Show a) => MimeRender CSS a where
  mimeRender _ val = encodeUtf8 $ pack $ show val
