{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts#-}

module Draw (filepath, draw) where

import Diagrams.Prelude
import Diagrams.TwoD.Size
--import Diagrams.Backend.SVG
--import Diagrams.Backend.Html5 can't install package
import Diagrams.Backend.Rasterific

myCircle :: Diagram B
myCircle = circle 1

sierpinski 1 = eqTriangle 1
sierpinski n =     s
                  ===
               (s ||| s) # centerX
  where s = sierpinski (n-1)

example :: Int -> Diagram B
example zoom = sierpinski zoom # center # lw none # fc black

filepath = "images/output.png" 

--draw zoom = renderSVG filepath (mkWidth 400) (example zoom # frame 0.1) --not using svg atm bc safari bad at displaying them
-- draw = renderHtml5 "output.html" (mkWidth 400) myCircle
draw zoom= renderRasterific filepath (mkWidth 400) (example zoom # frame 0.1)