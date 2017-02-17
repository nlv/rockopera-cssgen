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

data Config = Config {
            debug :: Bool
           ,rootFontSizeLarge :: Integer
           ,rootFontSizeOther :: Integer
           {-
           , topMenuCount :: Int
           ,mainContentWidthRatio :: Int
           ,mainSectionTopMargin :: Size Abs
           ,mainSectionsPadding :: Size Abs
           ,mainNavPadding :: Size Abs
           ,maindivPadding :: Size Abs
           ,subdivPadding :: Size Abs

           ,pMargin :: Size Rel
           -}
}


main :: IO ()
main = putCss $ do

  let cfg = Config {
              debug = True
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
  mainCSS cfg
  largeDesktopCSS cfg
  mediumDesktopCSS cfg
  smallDesktopCSS cfg
  largePhoneCSS cfg
  smallPhoneCSS cfg

largeDesktop :: Css -> Css
largeDesktop = query Clay.all [Media.minWidth 1200]

mediumDesktop :: Css -> Css
mediumDesktop = query Clay.all [Media.minWidth 992, Media.maxWidth 1199]

smallDesktop :: Css -> Css
smallDesktop = query Clay.all [Media.minWidth 768, Media.maxWidth 991]

largePhone :: Css -> Css
largePhone = query Clay.all [Media.minWidth 481, Media.maxWidth 767]

smallPhone :: Css -> Css
smallPhone = query Clay.all [Media.maxWidth 480 ]

largeDesktopCSS cfg = largeDesktop $ do 
    -- html ? fontSize (px $ fromInteger $ rootFontSizeLarge cfg)
    html ? fontSize (px 17)
    body |> main_ |> main_ ? do      -- DEBUG
          minWidth (px 600)          -- DEBUG
          maxWidth (px 600)          -- DEBUG

mediumDesktopCSS cfg = mediumDesktop $ do
    html ? fontSize (px 15)
    body |> main_ |> main_ ? do      -- DEBUG
          minWidth (px 500)          -- DEBUG
          maxWidth (px 500)          -- DEBUG

smallDesktopCSS cfg = smallDesktop $ do
    html ? fontSize (px 15)
    body |> main_ |> main_ ? do      -- DEBUG
          minWidth (px 390)          -- DEBUG
          maxWidth (px 390)          -- DEBUG
    bodyNarrow
    mainNavNarrow

largePhoneCSS cfg = largePhone $ do
    html ? fontSize (px 12)
    body |> main_ |> main_ ? do      -- DEBUG
          minWidth (px 390)          -- DEBUG
          maxWidth (px 390)          -- DEBUG
    bodyNarrow
    mainNavNarrow

smallPhoneCSS cfg = smallPhone $ do
    html ? fontSize (px 12)
    body |> main_ |> main_ ? do      -- DEBUG
          minWidth (px 240)          -- DEBUG
          maxWidth (px 240)          -- DEBUG
    bodyNarrow
    mainNavNarrow
    
mainNavNarrow = header |> nav |> ul |> li ? sym2 padding (Size.rem 1) (Size.rem 1)

bodyNarrow = body ? do
      width (vw 100)
      maxWidth (vw 100)



mainCSS cfg = do
    importUrl "font-awesome.min.css"
    importUrl "http://fonts.googleapis.com/css?family=Roboto Slab:100,100italic,300,300italic,400,400italic"

    html ? do
      boxSizing borderBox

      fontFamily ["Roboto"] [serif]

    body ? do
      width (vw 90)
      maxWidth (vw 90)
      sym margin auto

      display flex
      flexDirection column

      debugBox cfg red       -- DEBUG

      main_ <? do
        display flex
        justifyContent center

        debugBox cfg green   -- DEBUG
        
        main_ <? do
          debugBox cfg black         -- DEBUG
          section <? display none    -- DEBUG

        nav <? debugBox cfg blue     -- DEBUG


    header ? do
      debugBox cfg blue              -- DEBUG

      nav <? do
        display flex
        justifyContent center

        ul <? do
          display flex
          flexWrap Flexbox.wrap
          justifyContent center

          li ? do
            a ? fontWeight bold
            sym2 padding (Size.rem 1) (Size.rem 2)
      

    nav ? a ? do
        textDecoration none
        color black

    main_ |> nav # ".parts-links" ? do
      debugBox cfg yellow             -- DEBUG

      flexBasis (pct 25)
      flexGrow 1
      flexShrink 1

      sym padding (Size.em 1)
      
      ul <? li <? do
        paddingTop (Size.em 1)

        lineHeight (Size.em 1.5)

        p <? do
          
          color blue
          fontWeight bold

          borderBottomColor blue
          borderBottomStyle solid
          borderBottomWidth (px 1)

        debugBox cfg orange              -- DEBUG

    
    "#nav-left" ? do
      textAlign (alignSide sideRight)

      "ul" <? li ? do
        alignItems flexEnd

{- !!!

    {-- основные разделы --}
    "#main" ? do
      display flex
      justifyContent spaceAround

      marginTop (mainSectionTopMargin cfg)

      nav ? do
        debugBox cfg blue
        maxWidth (pct 25)

--        flexGrow 1
        sym padding (mainSectionsPadding cfg)

      "#content" ? do
        debugBox cfg blue

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
        debugBox cfg red

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
--
     

debugIt cfg action  = if (debug cfg) then action else return ()


debugBox cfg color = debugIt cfg $ box color
    where box color = do
              borderStyle solid
              borderColor color
              borderWidth (px 1)

