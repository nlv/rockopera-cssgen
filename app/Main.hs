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


import Prelude hiding (repeat)

main :: IO ()
main = putCss $ do

  reset
  commonCSS defaultConfig
  mainCSS defaultConfig

mainCSS cfg = do


    "#intro-video" ? textAlign center



--    "#main-content" |> section ? do
    "#intro" ? do  
--      marginTop (Size.em 2)
      sym2 padding 0 (Size.em 1) 

      textAlign center
      lineHeight (Size.em 1.5)
      maxWidth (px 700)
      sym2 margin (Size.em 0) auto 

      h1 <? do
        textAlign center
        fontWeight bold
        fontSize (Size.em 1.5)
        sym2 margin (Size.em 1) auto

      p ? do
        sym2 margin (Size.em 1) auto
        textAlign justify
      

    "#main-content" |> section # ":first-child" ? do
      marginTop (Size.em 0)

    "#protests" |> figure ? do
       display flex
       justifyContent spaceBetween
       alignItems stretch

       figure ? do
         flexGrow 1
         flexBasis (pct 33.33)
         position relative

         paddingLeft (px 5)
         paddingRight (px 5)

         img ? do
           width (pct 100)
           height (pct 100)

         figcaption ? do
           position absolute
           left (Size.em 0.5)
           bottom (Size.em 0.5)
           color lightgray
           backgroundColor gray
           sym padding (Size.em 0.25)

    "#news" ? do
      maxWidth (px 700)
      marginLeft auto
      marginRight auto

      h1 ? do
        fontWeight bold
        fontSize (Size.em 1.2)
        margin (Size.em 1) auto (Size.em 0.5) auto 
        textAlign center

--      article #  ":nth-of-type(1)" ? do
--        paddingTop (px 0)


      article ? do
        fontSize (Size.rem 0.9)
        borderBottom solid (px 1) black
        display flex
        flexDirection column

--        marginTop (Size.em 1.5)

        paddingLeft (px 5)
        paddingRight (px 5)
        paddingTop (Size.em 2.5)
        paddingBottom (Size.em 2.5)

        figure ? do
          float floatLeft
          maxWidth (pct 25)
          paddingLeft (Size.em 1)
          paddingRight (Size.em 1)
 --         sym padding (Size.em 1)
          
          img ? maxWidth (pct 100)

        hgroup ? do
--          paddingTop (Size.em 1)
          textAlign justify
          lineHeight (Size.em 1.5)


          h1 ? do
            fontWeight bold
            fontSize (Size.em 1.2)
            sym2 margin (Size.em 0) auto
            textAlign justify
            
            a ? textDecoration none

          h2 ? do
            sym2 margin (Size.em 1) auto

        footer ? do
          lineHeight (Size.em 1.2)
          width (pct 100)
--          display flex

          time ? do
            sym2 margin (px 0) (px 10)
            display inlineBlock
--            flexShrink 1
--


          ul ? do
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

