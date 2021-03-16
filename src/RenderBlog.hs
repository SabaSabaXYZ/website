module RenderBlog (renderBlog) where

import CMark (commonmarkToHtml)
import Lucid (Html(..), toHtmlRaw)
import qualified Data.Text as T

renderBlog :: T.Text -> Html ()
renderBlog = toHtmlRaw . commonmarkToHtml []
