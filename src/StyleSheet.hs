module StyleSheet where

import Clay
import Data.Monoid
import Prelude hiding (div, rem)

type ColorAction = Float -> Color -> Color

darkStyle :: Color -> Css
darkStyle = makeStyle lighten

lightStyle :: Color -> Css
lightStyle = makeStyle darken

makeStyle :: ColorAction -> Color -> Css
makeStyle colorAction themeColor = do
  html ? maxWidth (pct 100)
  bodyStyle colorAction themeColor
  paragraphStyle
  codeStyle
  headerStyle
  linkStyle colorAction themeColor
  navigationStyle

bodyStyle :: ColorAction -> Color -> Css
bodyStyle action themeColor = body ? do
  backgroundColor themeColor
  fontColor $ action 0.75 themeColor
  fontFamily ["Helvetica"] [sansSerif]
  fontWeight $ weight 300
  margin (px 0) auto auto auto
  maxWidth $ rem 48
  padding (rem 0.25) (rem 0.25) (rem 0.25) (rem 0.25)
  fontSize $ px 16

paragraphStyle :: Css
paragraphStyle = p ? do
  fontSize $ rem 1
  marginBottom $ rem 1.3

linkStyle :: ColorAction -> Color -> Css
linkStyle action themeColor = do
  let regularColor = fontColor $ action 0.60 $ themeColor +. 0x40
  let visitedColor = fontColor $ action 0.60 $ themeColor -. 0x40
  let highlightedColor = fontColor $ action 0.75 $ themeColor +. 0x50
  a ? regularColor
  a # visited ? visitedColor
  a # hover ? highlightedColor
  a # focus ? highlightedColor
  a # active ? highlightedColor

headerStyle :: Css
headerStyle = do
  h1 ? fontSize (rem 3.998)
  h2 ? fontSize (rem 2.827)
  h3 ? fontSize (rem 1.999)
  h4 ? fontSize (rem 1.414)
  h5 ? fontSize (rem 1.121)
  h6 ? fontSize (rem 0.88)
  small ? fontSize (em 0.707)
  h1 ? largeHeaderStyle
  h2 ? largeHeaderStyle
  h3 ? largeHeaderStyle
  where
    largeHeaderStyle = do
      margin (rem 1.414) (rem 0) (rem 0.5) (rem 0)
      paddingBottom $ rem 0.5
      textAlign center

codeStyle :: Css
codeStyle = do
  (pre <> code) ? fontFamily ["Courier New"] [monospace]
  pre ? do
    padding (rem 0.5) (rem 0.5) (rem 0.5) (rem 0.5)
    overflowX scroll

navigationStyle :: Css
navigationStyle = do
  div # ("role" @= "navigation") |> h2 ? smallHeaderStyle
  where
    smallHeaderStyle = do
      fontSize $ rem 1.414
      paddingBottom $ rem 0
      textAlign inherit
