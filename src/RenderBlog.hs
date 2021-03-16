module RenderBlog (renderBlog) where

import CMark (Node(..), CMarkOption(..), nodeToHtml, commonmarkToNode, optSafe)
import Lucid (Html(..), toHtmlRaw)
import qualified Data.Text as T

renderBlog :: T.Text -> Html ()
renderBlog = renderNode . cssToNode

cmarkOptions :: [CMarkOption]
cmarkOptions = [optSafe]

cssToNode :: T.Text -> Node
cssToNode = commonmarkToNode cmarkOptions

renderNode :: Node -> Html ()
renderNode = toHtmlRaw . nodeToHtml cmarkOptions
