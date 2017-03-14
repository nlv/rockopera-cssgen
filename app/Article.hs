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
    html ? fontSize (px 17)

    ".video-large" ? display inline
    ".video-medium" ? display none

mediumDesktopCSS cfg = mediumDesktop $ do
    html ? fontSize (px 15)

    ".video-large" ? display none
    ".video-medium" ? display inline

smallDesktopCSS cfg = smallDesktop $ do
    html ? fontSize (px 15)
    bodyNarrow
    mainNavNarrow

    ".video-large" ? display none
    ".video-medium" ? display inline

largePhoneCSS cfg = largePhone $ do
    html ? fontSize (px 12)
    bodyNarrow
    mainNavNarrow

    ".video-large" ? display none
    ".video-medium" ? display inline

smallPhoneCSS cfg = smallPhone $ do
    html ? fontSize (px 12)
    body |> main_ |> main_ ? do      -- DEBUG
          minWidth (px 240)          -- DEBUG
          maxWidth (px 240)          -- DEBUG
    bodyNarrow
    mainNavNarrow

    ".video-large" ? display none
    ".video-medium" ? display inline

mainNavNarrow = header |> nav |> ul |> li ? sym2 padding (Size.rem 1) (Size.rem 1)

bodyNarrow = do
      body ? do
        width (vw 100)
        maxWidth (vw 100)

      bodyColumnDirection


bodyColumnDirection = do
      body ? do
        main_ <? do
          flexDirection column
   
      "#main-content" ? do
        order 1
         
      "#nav-left" ? do 
        order 2
        textAlign center

--        display none

      "#nav-right" ? do
        order 3
        textAlign center

  --      display none


      main_ |> nav # ".parts-links" |> ul ? do
        li <? do p <? borderBottomStyle none
--        ul ? display none

      "#rubricator" ? display inline

      "#nav-left" # ":target" ? do
        ".part-links" ? display block

        

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
          {-
          section <? display none    -- DEBUG
          "#intro-video" ? display block -- DEBUG
          "#intro" ? display block -- DEBUG
          "#protests" ? display block -- DEBUG
          -}

          flexBasis (pct 50)         

        nav <? debugBox cfg blue     -- DEBUG

    "#intro-video" ? textAlign center

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
      maxWidth (px 500)

--      flexBasis (pct 25)
--      flexGrow 1
--      flexShrink 1

      paddingLeft (Size.em 1)
      paddingRight (Size.em 1)
      
      ul <? li <? do
        paddingTop (Size.em 1.5)
        lineHeight (Size.em 1.2)

        p <? a ? do
          
          color blue
          fontWeight bold

          borderBottomColor blue
          borderBottomStyle solid
          borderBottomWidth (px 1)

        ul <? li <? do
          paddingTop (Size.em 0.8)

        ul <? li # ":first-child" ? do
          paddingTop (Size.em 0.5)

        debugBox cfg orange              -- DEBUG

      ul <? li # ":first-child" ? paddingTop (px 0)

    
    "#nav-left" ? do
      textAlign (alignSide sideRight)

      "ul" <? li ? do
        alignItems flexEnd

    "#rubricator" ? display none

    "#main-content" |> section ? do
      marginTop (Size.em 4)

--    "#main-content" |> section ? do
    "#intro" ? do  
--      marginTop (Size.em 2)
      sym2 padding 0 (Size.em 1) 

      textAlign center
      lineHeight (Size.em 1.5)
      maxWidth (px 700)
      sym2 margin (Size.em 0) auto 

      typo
{-
      h1 ? do
        textAlign center
        fontWeight bold
        fontSize (Size.em 1.5)
        sym2 margin (Size.em 1) auto

      p ? do
        sym2 margin (Size.em 1) auto
        textAlign justify
        -}

      

    "#main-content"  ? do
      maxWidth (px 700)
      marginTop (Size.em 0)
      lineHeight (Size.em 1.5)

      h1 ? do
        textAlign center
        fontWeight bold
        fontSize (Size.em 1.5)
        sym2 margin (Size.em 1) auto

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

      Elements.em ? fontStyle italic

      figure ? do
        float floatLeft
        sym margin (px 10)



     
typo = do
      h1 ? do
        textAlign center
        fontWeight bold
        fontSize (Size.em 1.5)
        sym2 margin (Size.em 1) auto

      p ? do
        sym2 margin (Size.em 1) auto
        textAlign justify

debugIt cfg action  = if (debug cfg) then action else return ()


debugBox cfg color = debugIt cfg $ box color
    where box color = do
              borderStyle solid
              borderColor color
              borderWidth (px 1)
