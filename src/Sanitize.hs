module Sanitize where

import Lucid (Html(..), renderText, toHtmlRaw)
import Text.HTML.TagSoup.Tree (parseTree, renderTree, transformTree, TagTree(..))
import qualified Data.Text.Lazy as T

sanitizeHtml :: Html () -> Html ()
sanitizeHtml = toHtmlRaw . renderTree . transformTree sanitizeTree . parseTree . renderText

sanitizeTree :: TagTree T.Text -> [TagTree T.Text]
sanitizeTree (TagBranch "pre" attributes children) = [TagBranch "pre" (("tabindex", "0") : attributes) children]
sanitizeTree x = [x]
