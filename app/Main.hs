{-# LANGUAGE OverloadedStrings #-}
module Main where

import CSS.Reset
import CSS.Common
import Clay
import Clay.Display as Display
import Clay.Elements as Elements
import Clay.Size as Size
-- import Clay.Flexbox
-- import Data.Monoid

import Prelude hiding (repeat)

data Config = Config {
            topMenuCount :: Int
           ,debug :: Bool
           ,rootFontSize :: Integer
           ,mainContentWidthRatio :: Int
           ,mainSectionTopMargin :: Size Abs
           ,mainSectionsPadding :: Size Abs
           ,mainNavPadding :: Size Abs
           ,maindivPadding :: Size Abs
           ,subdivPadding :: Size Abs

           ,pMargin :: Size Rel
}


main :: IO ()
main = putCss $ do

  let cfg = Config {
              topMenuCount = 8
             ,debug = True
             ,rootFontSize = 17
             ,mainContentWidthRatio = 4
             ,mainSectionTopMargin = px 10
             ,mainSectionsPadding = px 60
             ,mainNavPadding = px 10
             ,maindivPadding = px 5
             ,subdivPadding = px 5
             ,pMargin = Size.rem 1
             }

  reset
  mainCSS cfg


mainCSS cfg = do
    importUrl "font-awesome.min.css"
    importUrl "http://fonts.googleapis.com/css?family=Roboto Slab:100,100italic,300,300italic,400,400italic"

    html ? do
      boxSizing borderBox

      fontFamily ["Roboto"] [serif]
      fontSize (px $ fromInteger $ rootFontSize cfg)

    body ? do
      width (vw 90)
      maxWidth (vw 90)
      sym margin auto

      display flex
      flexDirection column

      debugBox red

      main_ <? display none

    header ? do
      debugBox blue

      nav <? do
        display flex
        justifyContent center

        ul <? do
          display flex

          li ? do
            sym2 padding (Size.rem 1) (Size.rem 2)
            a ? fontWeight bold
      

    nav ? a ? do
        textDecoration none
        color black

{- !!!
    {-- главное меню --}
    header |> nav ? li ? do
      float floatLeft
      width (pct (100 / (fromIntegral $ (topMenuCount cfg + 1))))
      sym padding (px 5)
      textAlign (other "center")
  
      debugBox red

      a ? do
        textDecoration none
        fontWeight bold
        color black

    ".clearfix" # after ? do
        content $ stringContent ""
        display Display.table
        clear both

    {-- основные разделы --}
    "#main" ? do
      display flex
      justifyContent spaceAround

      marginTop (mainSectionTopMargin cfg)

      nav ? do
        debugBox blue
        maxWidth (pct 25)

--        flexGrow 1
        sym padding (mainSectionsPadding cfg)

        ".maindiv" ? do
          color blue

          paddingBottom (maindivPadding cfg)
          borderBottomColor blue
          borderBottomStyle solid
          borderBottomWidth (px 1)

        ".subdivs" |> li ? do
--          paddingBottom (subdivPadding cfg)
          paddingTop (subdivPadding cfg)

--        ".subdivs" |> li # lastChild ? do



      nav |> ul |> li ? do
        paddingTop (mainNavPadding cfg)
        paddingBottom (mainNavPadding cfg)

      "#nav-left" ? do
        textAlign (alignSide sideRight)

      "#nav-right" ? do
        textAlign (alignSide sideLeft)

      "#content" ? do
        debugBox blue

        -- marginLeft auto
        maxWidth (px 600)
        textAlign center

        flexGrow $ mainContentWidthRatio cfg
        sym padding (mainSectionsPadding cfg)

        lineHeight $ unitless 1.6


      {- Типографика -}
      "#content" ? p ? do
        marginTop $ pMargin cfg

        textAlign justify

      Elements.div # ".iframe" ? do
        debugBox red

      article ? header ? do
        fontSize (Size.em 1.4)
        textAlign (alignSide sideCenter)
        fontWeight bold
        marginTop $ pMargin cfg


    {- Карусель -}
    ".carusel" ? do
       display flex
       justifyContent spaceBetween
       alignItems stretch

       marginTop (Size.em 2)

       li ? do
         flexGrow 1
         flexBasis (pct 33.33)

         paddingLeft (px 5)
         paddingRight (px 5)

         img ? do
           width (pct 100)
           height (pct 100)

    {- новости -}
    "#news" ? do

      article ? do
        border solid (px 1) black
        display flex

        marginTop (Size.em 2)
        marginBottom (Size.em 2)

        sym padding (px 5)

        ".logo" ? do
          display flex
          flexDirection column
--        alignItems stretch

          Elements.div # ".date" ? do
            textAlign start
            fontSize (Size.em 0.8)

        section # ".title" ? do
          textAlign justify

          h1 ? do
            fontWeight bold
            lineHeight (Size.em 1.2)

          h2 ? do
            marginTop (Size.em 1)
            fontSize (Size.em 0.8)
            lineHeight (Size.em 1.2)




      ".photo" ? do
--        float floatLeft
--        width (pct 25)
        sym padding (px 5)

      ".photo" ? img ? do
        width (pct 100)
--}
     

  

     where debugIt action = 
              if (debug cfg) then action else return ()

           box color = do
              borderStyle solid
              borderColor color
              borderWidth (px 1)

           debugBox color = debugIt $ box color

