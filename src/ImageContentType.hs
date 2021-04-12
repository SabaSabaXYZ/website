module ImageContentType where

import Data.ByteString.Lazy (ByteString(..))
import Network.HTTP.Media ((//), (/:))
import Servant

data IMG

instance Accept IMG where
  contentType _ = "image" // "*"

instance MimeRender IMG ByteString where
  mimeRender _ val = val
