{-# LANGUAGE OverloadedStrings #-}
module CSS.Common (

     Config(..)

    ,defaultConfig
    ,largeDesktop
    ,mediumDesktop
    ,smallDesktop
    ,largePhone
    ,smallPhone

    ,largeDesktopCSS
    ,mediumDesktopCSS
    ,smallDesktopCSS
    ,largePhoneCSS
    ,smallPhoneCSS
    ,mainNavNarrow
    ,bodyNarrow
    ,bodyColumnDirection
    ,commonCSS
    ,debugIt
    ,debugBox
      )


where

import CSS.Reset
import Clay
import Clay.Display as Display
import Clay.Elements as Elements
import Clay.Size as Size
import Clay.Flexbox as Flexbox (wrap)
import qualified Clay.Media as Media


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


defaultConfig  = Config {
              debug = False
             ,rootFontSizeOther = 15
             ,rootFontSizeLarge = 19
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

largeDesktop :: Css -> Css
largeDesktop = query Clay.all [Media.minWidth 1400]

mediumDesktop :: Css -> Css
mediumDesktop = query Clay.all [Media.minWidth 992, Media.maxWidth 1399]

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

{-
    ".video-large" ? do
              borderStyle solid
              borderColor red
              borderWidth (px 5)
-}

mediumDesktopCSS cfg = mediumDesktop $ do
    html ? fontSize (px 15)

    ".video-large" ? display none
    ".video-medium" ? display inline

{-
    ".video-medium" ? do
              borderStyle solid
              borderColor green
              borderWidth (px 5)
              -}

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

        

commonCSS cfg = do
    importUrl "font-awesome.min.css"
    importUrl "http://fonts.googleapis.com/css?family=Roboto Slab:100,100italic,300,300italic,400,400italic"
    importUrl "http://fonts.googleapis.com/css?family=Bad Script:100,100italic,300,300italic,400,400italic"

    main_ # ".teasers-list" <? header <? h1 ? do
      fontSize (Size.em 1.2)
      color red
      color green
      textTransform uppercase

    main_ # ".teasers-list" <? header <? h1 # ":before" ? do
        content (stringContent "#")
        fontSize (Size.em 1.2)
        fontWeight bold


    article # ":last-child" ? borderBottom solid (px 0) black

    article # ".teaser" <? do
      fontSize (Size.rem 0.9)
      borderBottom solid (px 1) black
      display flex
      flexDirection column

      paddingLeft (px 5)
      paddingRight (px 5)
      paddingTop (Size.em 2.5)
      paddingBottom (Size.em 2.5)


      figure ? do
        position relative
        float floatLeft
        maxWidth (pct 25)
        paddingLeft (Size.em 1)
        paddingRight (Size.em 1)
        img ? maxWidth (pct 100)

      figure # ".video" ? do
        position relative

      figure # ".video" # ":before" ? do
        display block
        position absolute
        content (stringContent "")
        backgroundImage (url "../images/icons/movie.png")
        backgroundRepeat noRepeat
        backgroundSize $ (pct 30) `by` auto
        backgroundPosition (sideCenter `placed` sideCenter)
        zIndex 1
        left (px 0)
        top (px 0)
        width (pct 100)
        height (pct 100)

      hgroup ? do
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

        time ? do
          sym2 margin (px 0) (px 10)
          display inlineBlock

    article # ":last-child" ? borderBottom solid (px 0) black

    ul # ".keywords" ? do 
      display flex
      flexWrap Flexbox.wrap
      alignContent spaceBetween

      li ? do 
        display inlineBlock
        marginTop (Size.px 10)
        marginLeft (px 10)
        marginRight (px 10)

        textTransform uppercase
        whiteSpace nowrap
        color red
        color green

{-
        sym padding (px 6)
        backgroundColor lemonchiffon
        sym borderRadius (px 4)
        -}

        a ? do
          textDecoration none
          color inherit

      li # ":last-child" ? do 
        marginRight (px 0)

    ul # ".keywords" <? li # ":before" ? do 
      content (stringContent "#")
      fontSize (Size.em 1.2)
      fontWeight bold
      


    html ? do
      boxSizing borderBox

      fontFamily ["Roboto"] [serif]

    body ? do
      width (vw 90)
      maxWidth (vw 90)
      sym margin auto

      display flex
      flexDirection column

--      debugBox cfg red       -- DEBUG


      main_ <? do
        display flex
--        justifyContent spaceBetween
        justifyContent spaceAround
        alignItems flexStart

        marginBottom (Size.em 2)

--        debugBox cfg green   -- DEBUG
        
        main_ <? do
--          debugBox cfg black         -- DEBUG
          width (pct 50)         

        nav <? debugBox cfg blue     -- DEBUG

      header <? do
        debugBox cfg blue              -- DEBUG
        sym2 padding (Size.em 2) 0

        nav <? do
          fontSize (Size.rem 1.2)
          display flex
          justifyContent center
          backgroundColor lightgray

          ul <? do
            display flex
            flexWrap Flexbox.wrap
            justifyContent center

            li ? do
              a ? fontWeight bold
              sym2 padding (Size.rem 1) (Size.rem 2)
      
          a ? do
            textDecoration none
            color black

      footer <? do
        width (pct 100)
        backgroundColor gray
        display flex
        sym2 padding (Size.em 1) 0
        lineHeight (Size.em 1.5)

{-
      footer <? do
        justifyContent spaceAround
        -}
      footer <? do
        justifyContent center

        section <? sym2 margin 0 (px 100)

        a ? do
          textDecoration none


    ".parts-links" ? do
      maxWidth (px 300)
      width (pct 25)
      sym padding  (Size.em 0.5)

      backgroundColor whitesmoke

      
      ul <? li <? do
        paddingTop (Size.em 1.5)
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

    "#nav-right" ? do
      marginLeft (Size.em 2)
    
    "#nav-left" ? do
      marginRight (Size.em 2)

      "ul" <? li ? do
        alignItems flexEnd

    "#rubricator" ? display none

    "#main-content" ? do
      backgroundColor whitesmoke
      sym2 padding  (Size.em 1) (Size.em 2)
      alignSelf stretch

      section <? do
        marginTop (Size.em 4)



    largeDesktopCSS cfg
    mediumDesktopCSS cfg
    smallDesktopCSS cfg
    largePhoneCSS cfg
    smallPhoneCSS cfg






     

debugIt cfg action  = if (debug cfg) then action else return ()


debugBox cfg color = debugIt cfg $ box color
    where box color = do
              borderStyle solid
              borderColor color
              borderWidth (px 1)

