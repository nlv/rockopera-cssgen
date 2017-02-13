module CSS.Common ( 
           inheritFont
         , zero
         , paddingAll
    ) where

import Clay

inheritFont = Required inherit Nothing [] []

zero = px 0

paddingAll s = padding s s s s
