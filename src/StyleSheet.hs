module StyleSheet where

import Clay

type ColorAction = Float -> Color -> Color

darkStyle :: Color -> Css
darkStyle = makeStyle lighten

lightStyle :: Color -> Css
lightStyle = makeStyle darken

makeStyle :: ColorAction -> Color -> Css
makeStyle colorAction themeColor = do
  bodyStyle colorAction themeColor
  linkStyle colorAction themeColor

bodyStyle :: ColorAction -> Color -> Css
bodyStyle action themeColor = body ? do
  backgroundColor themeColor
  fontColor $ action 0.75 themeColor

linkStyle :: ColorAction -> Color -> Css
linkStyle action themeColor = a ? do
  fontColor $ action 0.60 $ themeColor +. 0x40
