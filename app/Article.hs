{-# LANGUAGE OverloadedStrings #-}
module Main where

import CSS.Reset
import CSS.Common
import Clay
import Clay.Display as Display
import Clay.Elements as Elements
import Clay.Size as Size
import Clay.Flexbox as Flexbox (wrap)
import qualified Clay.Media as Media

-- import Clay.Flexbox
-- import Data.Monoid

import Prelude hiding (repeat)



main :: IO ()
main = putCss $ do

  let cfg = Config {
              debug = False
             ,rootFontSizeOther = 15
             ,rootFontSizeLarge = 17
             {-
             ,topMenuCount = 8
             ,mainContentWidthRatio = 4
             ,mainSectionTopMargin = px 10
             ,mainSectionsPadding = px 60
             ,mainNavPadding = px 10
             ,maindivPadding = px 5
             ,subdivPadding = px 5
             ,pMargin = Size.rem 1
             -}
             }

  reset
  commonCSS defaultConfig
  mainCSS cfg

mainCSS cfg = do

    blockquote # ".poem" ? do
        fontStyle italic
        marginTop (Size.em 1)
        marginLeft (Size.em 2)
        

    "#main-content" ? figure # ".citation" ? do
        lineHeight (Size.em 1.5)
        fontSize (Size.em 1.2)
        fontFamily ["Bad Script"] [serif]
        fontStyle italic
        marginTop (Size.em 1.5)
        marginBottom (Size.em 1.5)
        marginLeft (Size.em (-2.5))
        marginRight (Size.em (-2.5))
        sym2 padding (Size.em 1.5) (Size.em 1)
        textIndent $ indent (Size.em 2)

        backgroundColor lightgray
      
        blockquote <? do
          textAlign justify
          
          p # ":first-child" ? do
            marginTop (px 0)

        figcaption <? do
          marginTop (Size.em 1)
          textAlign end
          fontWeight bold

    ul # ".keywords" ? do
  --            float floatRight
  --            flexShrink 4
      display flex
      flexWrap Flexbox.wrap
      alignContent spaceBetween
  --            sym2 margin (px 0) (px 10)

      li ? do
        display inlineBlock
  --              float floatRight
  --              sym2 margin (Size.em 1.2) (px 10)
  --              marginTop (Size.em 1)
        marginTop (Size.px 10)
        marginLeft (px 10)
        marginRight (px 10)
        sym padding (px 4)

        backgroundColor lemonchiffon
        sym borderRadius (px 4)

  --            li # ":first-child" ? do
  --              marginLeft (px 0)

      li # ":last-child" ? do
        marginRight (px 0)

    "#main-content"  ? do
      maxWidth (px 700)
      width (pct 60)
      marginTop (Size.em 0)
      lineHeight (Size.em 1.5)

      h1 ? do
 --       textAlign center
        fontWeight bold
        fontSize (Size.em 1.5)
        sym2 margin (Size.em 1) auto
        lineHeight (Size.em 1.1)

      h2 ? do
        textAlign justify
--        fontWeight bold
        fontSize (Size.em 1.1)
        fontStyle italic
        sym2 margin (Size.em 1) auto


      p ? do
        sym2 margin (Size.em 1) auto
        textAlign justify

      p # ".question" ? do
        fontWeight bold
        fontStyle italic

      Elements.em ? do
        fontStyle italic
        fontFamily ["Bad Script"] [serif]
--        backgroundColor lightgray

      figure ? do
        float floatLeft
        sym margin (px 10)

    aside <? do
      maxWidth (px 300)
      width (pct 25)
      marginLeft (Size.em 2)
      marginRight (Size.em 2)

      img # ".icon" ? do
        display inline
        width (Size.em 2)
        sym padding (px 2)
        verticalAlign middle


      section <? do
        backgroundColor whitesmoke
        sym padding  (Size.em 0.5)
        marginTop (Size.em 2)

        h1 ? do
          display none
   --       textAlign center
          fontWeight bold
--          sym2 margin (Size.em 1) auto
--          lineHeight (Size.em 1.1)

        ul # ".inline" <? do
          display flex
          justifyContent spaceAround
          flexWrap Flexbox.wrap


        ul <? li <? do
          paddingTop (Size.em 0.5)
          lineHeight (Size.em 1.2)

          p <? a ? do
            textDecoration none
            
            color blue
            fontWeight bold

          ul <? li <? do
            paddingTop (Size.em 0.4)

            a ? do
              textDecoration none
              color black

            a # ":hover" ? do
              color steelblue

        ul # ".first-child" <? li  <? do
          paddingTop (Size.em 1)


{-
    footer <? do
      maxWidth (px 700)
      width (pct 60)
      alignSelf center
      justifyContent spaceAround
      sym padding (px 0)
      sym margin (px 0)
      -}





     
