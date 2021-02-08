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
 matchingRule :: Edge f a -> Edge f a -> Bool  --Edge = (Point, Point)

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


instance Tiling Penrose where
  draw (Kite o)= kite o
  draw (Dart o)= dart o

--mkTiling -> [Point f a] -> [Bool] -> Tiling

kite :: Origin -> Diagram B
kite o = moveTo o $ moveOriginTo p2 (body <> matchingRules)
  where
    p1 = origin & _r .~ 1 & _theta .~ (36 @@ deg)
    p2 = origin & _r .~ 1 & _theta .~ (0 @@ deg)
    p3 = origin & _r .~ 1 & _theta .~ (-36 @@ deg)
    body = strokeLine (origin ~~ p1) <> (p1 ~~ p2) <> (p2 ~~ p3) <> (p3 ~~ origin)
    matchingRules = moveTo p1 mrWhite <> moveTo p3 mrWhite <> moveTo p2 mrBlack <> mrBlack

dart :: Origin -> Diagram B
dart o = moveTo o $ moveOriginTo p2 (body <> matchingRules)
  where
    p1 = origin & _r .~ 1 & _theta .~ (36 @@ deg)
    p2 = origin & _r .~ 1 & _theta .~ (0 @@ deg)
    p3 = origin & _r .~ 1 & _theta .~ (-36 @@ deg)
    p4 = p1 .+^ e (-36 @@ deg) -- (origin .-. p3) is the vector pointing from origin to p3
    body = strokeLine mempty <> (p2 ~~ p1) <> (p2 ~~ p3) <> (p3 ~~ p4) <> (p1 ~~ p4) -- the pointless mempty is bc for some reason the first line always draws weird
    matchingRules = moveTo p1 mrBlack <> moveTo p3 mrBlack <> moveTo p2 mrWhite <> moveTo p4 mrWhite

halfDart :: Origin -> Diagram B
halfDart o = moveTo o $ moveOriginTo p2 (body <> matchingRules)
  where
    p1 = origin & _r .~ 1 & _theta .~ (36 @@ deg)
    p2 = origin & _r .~ 1 & _theta .~ (0 @@ deg)
    p3 = origin & _r .~ 1 & _theta .~ (-36 @@ deg)
    p4 = p1 .+^ e (-36 @@ deg) -- (origin .-. p3) is the vector pointing from origin to p3
    body = strokeLine mempty <> (p2 ~~ p1) <> (p2 ~~ p4) <> (p1 ~~ p4)
    matchingRules = moveTo p1 mrBlack <> moveTo p2 mrWhite <> moveTo p4 mrWhite

halfKite :: Origin -> Diagram B
halfKite o = moveTo o $ moveOriginTo p2 (body <> matchingRules)
  where
    p1 = origin & _r .~ 1 & _theta .~ (36 @@ deg)
    p2 = origin & _r .~ 1 & _theta .~ (0 @@ deg)
    body = strokeLine mempty <> (p2 ~~ p1) <> (p2 ~~ origin) <> (p1 ~~ origin)
    matchingRules = moveTo p1 mrWhite  <> moveTo p2 mrBlack <> moveTo origin mrBlack


-- takes a list of points and bool
-- bools correspond to the matching rule, colour at that point, true for black false for white
-- each point is connected to the points next to it in the list
-- origin is set to the first point in list
--mkTiling :: [(Point v n, Bool)] -> Diagram B

mkTiling :: [(Point V2 Double, Bool)] -> QDiagram B V2 Double Any
mkTiling vertices = (body <> matchingRules ps mrs) # showOrigin
  where
    (ps, mrs) = unzip vertices
    body = moveTo (head ps) $ strokeLine $ fromVertices (ps ++ [head ps])
    matchingRules (b:bs) (p:ps)= mr b p <> matchingRules bs ps
    matchingRules [] [] = mempty

mr :: Point V2 Double -> Bool -> QDiagram B V2 Double Any
mr p True  = moveTo (p - origin ) mrBlack
mr p False = moveTo (p - origin ) mrWhite

mrWhite :: Diagram B
mrWhite = circle 0.05

mrBlack :: Diagram B
mrBlack = circle 0.05 # fc black

--Note the origin of this diagram is in the left most point


kite' = mkTiling vertices
  where
    p1 = origin & _r .~ 1 & _theta .~ (36 @@ deg)
    p2 = origin & _r .~ 1 & _theta .~ (0 @@ deg)
    p3 = origin & _r .~ 1 & _theta .~ (-36 @@ deg)
    vertices = zip [origin, p1, p2, p3] [True, False, True, False ]

dart' = mkTiling vertices
  where
    p1 = origin & _r .~ 1 & _theta .~ (36 @@ deg)
    p2 = origin & _r .~ 1 & _theta .~ (0 @@ deg)
    p3 = origin & _r .~ 1 & _theta .~ (-36 @@ deg)
    p4 = p1 .+^ e (-36 @@ deg) -- (origin .-. p3) is the vector pointing from origin to p3
    vertices = zip [p1, p2, p3, p4] [True, False , True, False]

halfKite' = mkTiling vertices
  where
    p1 = origin & _r .~ 1 & _theta .~ (36 @@ deg)
    p2 = origin & _r .~ 1 & _theta .~ (0 @@ deg)
    p3 = origin & _r .~ 1 & _theta .~ (-36 @@ deg)
    vertices = zip [origin, p1, p2] [True, False, True]

halfDart' = mkTiling vertices
  where
    p1 = origin & _r .~ 1 & _theta .~ (36 @@ deg)
    p2 = origin & _r .~ 1 & _theta .~ (0 @@ deg)
    p3 = origin & _r .~ 1 & _theta .~ (-36 @@ deg)
    p4 = p1 .+^ e (-36 @@ deg) -- (origin .-. p3) is the vector pointing from origin to p3
    vertices = zip [p1, p2, p4] [True, False, False]



 
--gTest = mainWith (tabl ||| torange ||| pange ||| sormeh_dah ||| shesh_band)
gTest = renderSVG "images/gTest.svg" (mkWidth 400) $ (kite origin ||| dart origin  ||| halfDart origin ||| halfKite origin ) === (kite' ||| dart' ||| halfKite' ||| halfDart')
--gTest = renderSVG "images/gTest.svg" (mkWidth 400) $ (kite origin # showOrigin ||| kite' # showOrigin)
