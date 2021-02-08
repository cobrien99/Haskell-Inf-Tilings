{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts#-}

module Girih ( gTest ) where
  
import Diagrams.Prelude hiding (dart, halfDart)
import Diagrams.Backend.SVG
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Vector ( e )

class Tiling a where --come up with better name, qTiling?
 draw :: a -> Diagram B
 --deflate :: a -> [a]

--This module contains Tilesets e.g. Girih

--The Girih tileset contains 5 tiles. Each tile has a strapwork line (Girih that deroates the tile). Each Girih line crosses the center of an edge at 54 deg

--a regular decagon with ten interior angles of 144°; (Tabl)
--an elongated (irregular convex) hexagon with interior angles of 72°, 144°, 144°, 72°, 144°, 144°; (Shesh Band)
--a bow tie (non-convex hexagon) with interior angles of 72°, 72°, 216°, 72°, 72°, 216°; (Sormeh Dan)
--a rhombus with interior angles of 72°, 108°, 72°, 108°; (Torange)
--a regular pentagon with five interior angles of 108°. (Pange)
--src: https://en.wikipedia.org/wiki/Girih_tiles

torange :: Diagram B
torange = polygon ( with & polyType .~ PolySides --technically only need n-2 angles and n-1 edges bu this is more readable
  [ 72 @@ deg, 108 @@ deg, 72 @@ deg, 108 @@ deg ]
  (replicate 4 1)
  ) --body shape
--  <> 
----  polygon ( with & polyType .~ PolySides
----  [ 72 @@ deg, 108 @@ deg, 72 @@ deg, 108 @@ deg ]
----  [ 1        , 1        , 1        , 1           ]
----  )
  -- Girih decoration
  
tabl :: Diagram B 
tabl = polygon ( with & polyType .~ PolySides
  (replicate 10 ((180-144.0) @@ deg)) (replicate 10 1.0)) --body shape
--this looks close to working, need to check somehow


pange :: Diagram B
pange = polygon (with & polyType .~ PolySides
  (replicate 5 ((180-108.0) @@ deg)) (replicate 5 1.0)) 
--looks pretty good

sormeh_dah :: Diagram B
sormeh_dah = polygon (with & polyType .~ PolySides
  (map ((@@ deg) . (180-)) [72,72,216,72,72,216]) 
  (replicate 6 1.0))
--looks grand

shesh_band :: Diagram B
shesh_band = polygon (with & polyType .~ PolySides
  (map ((@@ deg) . (180-)) [72,144,144,72,144,144])
  (repeat 1.0))  


--They're all broadly the right shape and size. Not sure How I can check more at this stage

data Penrose = Kite | Dart | HalfDart

instance Tiling Penrose where
  draw Kite = kite
  draw Dart = dart

kite :: Diagram B
kite = moveOriginTo p2 (body <> matchingRules)
  where
    p1 = origin & _r .~ 1 & _theta .~ (36 @@ deg)
    p2 = origin & _r .~ 1 & _theta .~ (0 @@ deg)
    p3 = origin & _r .~ 1 & _theta .~ (-36 @@ deg)
    body = strokeLine (origin ~~ p1) <> (p1 ~~ p2) <> (p2 ~~ p3) <> (p3 ~~ origin)
    matchingRules = moveTo p1 mrWhite <> moveTo p3 mrWhite <> moveTo p2 mrBlack <> mrBlack

dart :: Diagram B
dart = moveOriginTo p2 (body <> matchingRules)
  where
    p1 = origin & _r .~ 1 & _theta .~ (36 @@ deg)
    p2 = origin & _r .~ 1 & _theta .~ (0 @@ deg)
    p3 = origin & _r .~ 1 & _theta .~ (-36 @@ deg)
    p4 = p1 .+^ e (-36 @@ deg) -- (origin .-. p3) is the vector pointing from origin to p3
    body = strokeLine mempty <> (p2 ~~ p1) <> (p2 ~~ p3) <> (p3 ~~ p4) <> (p1 ~~ p4) -- the pointless mempty is bc for some reason the first line always draws weird
    matchingRules = moveTo p1 mrBlack <> moveTo p3 mrBlack <> moveTo p2 mrWhite <> moveTo p4 mrWhite

halfDart :: Diagram B
halfDart = moveOriginTo p2 (body <> matchingRules)
  where
    p1 = origin & _r .~ 1 & _theta .~ (36 @@ deg)
    p2 = origin & _r .~ 1 & _theta .~ (0 @@ deg)
    p3 = origin & _r .~ 1 & _theta .~ (-36 @@ deg)
    p4 = p1 .+^ e (-36 @@ deg) -- (origin .-. p3) is the vector pointing from origin to p3
    body = strokeLine mempty <> (p2 ~~ p1) <> (p2 ~~ p4) <> (p1 ~~ p4)
    matchingRules = moveTo p1 mrBlack <> moveTo p2 mrWhite <> moveTo p4 mrWhite


mrWhite :: Diagram B
mrWhite = circle 0.05

mrBlack :: Diagram B
mrBlack = circle 0.05 # fc black
 
--gTest = mainWith (tabl ||| torange ||| pange ||| sormeh_dah ||| shesh_band)
gTest = renderSVG "images/gTest.svg" (mkWidth 400) (kite ||| dart ||| halfDart)
