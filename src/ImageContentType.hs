module ImageContentType where

import Data.ByteString.Lazy (ByteString(..))
import Network.HTTP.Media ((//), (/:))
import Servant

data PNG

instance Accept PNG where
  contentType _ = "image" // "png"

instance MimeRender PNG ByteString where
  mimeRender _ val = val
