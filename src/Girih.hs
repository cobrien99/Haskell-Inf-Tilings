{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts#-}

module Girih ( gTest ) where
  
import Diagrams.Prelude hiding (dart, halfDart, dart', halfDart')
import Diagrams.Backend.SVG
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Vector ( e )

class Tiling a where --come up with better name, qTiling?
 draw :: a -> Diagram B
 --deflate :: a -> [a]
 --matchingRule :: Edge f a -> Edge f a -> Bool  --Edge = (Point, Point)
 vertices :: a -> [(Point V2 Double, Bool)]

type Edge f n = (Point f n, Point f n)
type Origin = Point V2 Double --not sure about this type sig but sure look. maybe scoped type fams?

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

data Penrose = Kite Origin | Dart Origin| HalfDart Origin | HalfKite Origin

--some important points for this tileset
-- pr stands for Penrose
-- the (.) is a reminder that this is a point
-- The number represents the point. broadly 0 is the leftmost point and they increase clockwise

pr0 = origin 
pr1 = origin & _r .~ 1 & _theta .~ (36 @@ deg)
pr2 = origin & _r .~ 1 & _theta .~ (0 @@ deg)
pr3 = origin & _r .~ 1 & _theta .~ (-36 @@ deg)
pr4 = pr1 .+^ e (-36 @@ deg)

instance Tiling Penrose where
  vertices (Kite o) =     map (moveOriginTo o) [pr0, pr1, pr2, pr3] `zip` [True, False, True, False]
  vertices (Dart o) =     map (moveOriginTo o) [pr2, pr3, pr4, pr1] `zip` [False, True, False, True]
  vertices (HalfKite o) = map (moveOriginTo o) [pr0, pr1, pr2]      `zip` [True, False, True]
  vertices (HalfDart o) = map (moveOriginTo o) [pr2, pr4, pr1]      `zip` [False, False, True]

  draw = mkTiling . vertices



-- takes a list of points and bool
-- bools correspond to the matching rule, colour at that point, true for black false for white
-- each point is connected to the points next to it in the list
-- origin is set to the first point in list
--mkTiling :: [(Point v n, Bool)] -> Diagram B

-- TODO first diagram (Kite) draws shifted to the right for some reason

mkTiling :: [(Point V2 Double, Bool)] -> QDiagram B V2 Double Any
mkTiling vertices = body <> translate (origin .-. head ps) (matchingRules ps mrs)
  where
    (ps, mrs) = unzip vertices
    body = strokeLine $ fromVertices (ps ++ [head ps])
    matchingRules (b:bs) (p:ps)= mr b p <> matchingRules bs ps
    matchingRules [] [] = mempty

mr :: Point V2 Double -> Bool -> QDiagram B V2 Double Any
mr p True  = circle 0.05 # fc black # moveTo p 
mr p False = circle 0.05 # moveTo p 
 
--gTest = mainWith (tabl ||| torange ||| pange ||| sormeh_dah ||| shesh_band)
gTest = renderSVG "images/gTest.svg" (mkWidth 400) $ foldr ((|||) . draw) mempty [Kite origin, Dart origin, HalfDart origin , HalfKite origin ]
