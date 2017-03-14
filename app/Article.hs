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



     
