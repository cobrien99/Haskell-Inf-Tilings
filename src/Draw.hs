{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts#-}
{-# LANGUAGE OverloadedStrings #-}

module Draw (filepath, draw) where

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Graphics.Svg.Core (renderText)

sierpinski :: Int -> Diagram B
sierpinski 1 = eqTriangle 1
sierpinski n =     s
                  ===
               (s ||| s) # centerX
  where s = sierpinski (n-1)

example :: Int -> Diagram B
example zoom = sierpinski zoom # center # lw none # fc black

filepath = "images/output.svg" 

draw zoom= renderText $ renderDia SVG (SVGOptions (mkWidth 600) Nothing "" [] True) $ example zoom