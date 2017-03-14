{-# LANGUAGE OverloadedStrings #-}
module CSS.Reset (
                 reset
    ) where

import Clay
import Data.Monoid

import Prelude hiding (div, span)

zero = px 0

inheritFont = Required inherit Nothing [] []

reset = do
    elements ? do
      sym margin zero
      sym padding zero
      border none zero none
      fontSize (pct 100)
      font inheritFont
      verticalAlign middle -- baseline

    foldr1 mappend [article, aside, details, figcaption, figure, footer, header, hgroup, menu, nav, section] ? display block

    ol <> ul ? listStyle none none none
        
    blockquote <> q ? ("quotes" -: "none")

    blockquote # before <>  blockquote # after <> q # before <>  q # after ? do
      content (stringContent "")
      content none

    table ? do
      borderCollapse collapse
      "border-spacing" -: "0"

  where elements = foldr1 mappend elements'
        elements' = [
                      a
                    , abbr
                    , element "acronym"
                    , address
                    , element "applet"
                    , article
                    , aside
                    , audio
                    , b
                    , element "big"
                    , blockquote
                    , body 
                    , canvas
                    , caption
                    , element "center"
                    , cite
                    , code
                    , dd
                    , del
                    , details
                    , dfn
                    , div 
                    , dl
                    , dt
                    , element "em"
                    ]
