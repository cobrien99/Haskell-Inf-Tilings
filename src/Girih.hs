{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts#-}

module Girih where
  
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.Backend.SVG.CmdLine

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


 
gTest = mainWith (tabl ||| torange ||| pange ||| sormeh_dah ||| shesh_band)
