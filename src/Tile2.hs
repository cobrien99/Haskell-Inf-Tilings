{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Tile2 (tileTest2, guiDemo, kiteCons, dartCons, tile, ) where

import Diagrams.Prelude hiding (dart, halfDart, dart', halfDart')
import Diagrams.Backend.SVG
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Vector ( e )
import Graphics.Svg.Core (renderText)



--The Bool describes the matching rule at the vertex. The Vector tells you where to go to get to the next vertex
type Vertex = (V2 Double, Bool)
type Edge = (V2 Double, (Bool, Bool))

--instance Eq [Vertex] where

--type Edge = (Vertex, Vertex)

-- class Tile a where
--     vertices :: a -> [Vertex]

--     draw :: a -> Diagram B
--     --draw = mkTiling . vertices

--     tileSides :: a -> Int
--     tileSides = length . vertices

--data Tile a = Tile {edges :: [V2 Double]}

data Tiling a = Single (Tile a) | Many [Tile a]

findMatchingedges :: [V2 Double] -> [V2 Double] -> [V2 Double]
findMatchingedges [] _ = []
findMatchingedges (v:vs) b = filter (\x -> norm v == norm x) b ++ findMatchingedges vs b




-- allPosibilities :: Tile a -> Tile a -> Tiling a
-- allPosibilities (Tile e1) (Tile e2) = 

-- instance Semigroup (Tiling a) where
--     Single t1 <> Single t1 = 


-- instance Semigroup Tile where
--     (Tile e1) <> (Tile e2) = Tile 

--data Tiling a = Tiling [Tile a]

data Penrose = Kite | Dart | HalfDart | HalfKite
kiteCons = Kite
dartCons = Dart

edgesP :: Penrose -> [Edge]
edgesP Kite = [(e (36 @@ deg), (True, False)), (-e (36 @@ deg) + e (0 @@ deg), (False, True)), (-e (0 @@ deg) + e (-36 @@ deg), (True, False))]


data Tile a = Tile a (Transformation V2 Double)
tile = Tile

getTransform :: Tile a -> Transformation V2 Double
getTransform (Tile _ t) = t


kite :: [V2 Double]
kite = [e (36 @@ deg), -e (36 @@ deg) + e (0 @@ deg), -e (0 @@ deg) + e (-36 @@ deg), -e (-36 @@ deg)]

dart :: [V2 Double]
dart = [e (-36 @@ deg), -e (36 @@ deg), -e (-36 @@ deg) + e (0 @@ deg), - e (0 @@ deg) + e (36 @@ deg)]
--dart = [ - e (0 @@ deg) + e (36 @@ deg), e (-36 @@ deg), -e (36 @@ deg), -e (-36 @@ deg) + e (0 @@ deg)]

halfKite :: [V2 Double]
halfKite = [e (36 @@ deg), -e (36 @@ deg) + e (0 @@ deg), -e (0 @@ deg)]

halfDart :: [V2 Double]
halfDart = [e (-36 @@ deg), -e (36 @@ deg) -e (-36 @@ deg) + e (0 @@ deg), - e (0 @@ deg) + e (36 @@ deg)]
--halfDart = [- e (0 @@ deg) + e (36 @@ deg), e (-36 @@ deg), -e (36 @@ deg) -e (-36 @@ deg) + e (0 @@ deg)]

mkTiling :: [Vertex] -> QDiagram B V2 Double Any
mkTiling vertices = body -- <> translate (origin .-. centroid ps) (matchingRules ps mrs)
  where
    (vs, mrs) = unzip vertices
    body = fromOffsets vs
    --body = translate (head ps .-. centroid ps) $ strokeLine $ fromVertices (ps ++ [head ps]) --from vertices sets the first point as the origin so have to correct for that
    -- matchingRules (b:bs) (p:ps)= mr b p <> matchingRules bs ps --matching rule treats (0,0) a orgin so have to correct for that too
    -- matchingRules [] [] = mempty

deflate :: Penrose -> QDiagram B V2 Double Any
deflate Kite = draw halfKite <> draw halfKite # reflectY
deflate Dart = draw halfDart === draw halfDart # reflectY
deflate HalfKite = draw kite # rotate (-108 @@ deg) # moveOriginBy (- e (36 @@ deg)) <> draw halfDart # rotate (-144 @@ deg) -- # alignT # centerX
deflate HalfDart = draw halfDart # rotate (144 @@ deg) <> draw halfKite # reflectX # alignL

draw x = mkTiling (map (,True) x)

draw' :: Tile Penrose -> QDiagram B V2 Double Any
draw' (Tile Kite t) = draw kite # transform t # showOrigin # lc yellow
draw' (Tile Dart t) = draw dart # transform t # showOrigin # lc green
draw' (Tile HalfKite t) = draw halfKite # transform t # showOrigin # lc blue
draw' (Tile HalfDart t) = draw halfDart # transform t # showOrigin # lc red

deflateNdraw :: Int -> Tile Penrose -> QDiagram B V2 Double Any
deflateNdraw 0 t = draw' t
deflateNdraw 1 t = transform (getTransform t) . foldr (matchingRule t . draw') mempty $ deflateTile' t--matchingRule t $ deflateTile' t
deflateNdraw 2 t = foldr (matchingRule t . deflateNdraw 1) mempty (deflateTile' t)
deflateNdraw n t = draw' t
--deflateNdraw t n = (matchingRule t) ()


--determines how the chuildren of this tile will line up when the tile is deflated
--matchingRule :: Tile Penrose -> ([Tile Penrose] -> QDiagram B V2 Double Any)
matchingRule (Tile Kite t) = (===) --foldr ((===) . draw') mempty
matchingRule (Tile Dart t) = (===) --foldr ((===) . draw') mempty
matchingRule (Tile HalfKite t) = beside (e (270-18 @@ deg)) --foldr (beside (e (108 @@ deg)) . draw') mempty
matchingRule (Tile HalfDart t) = (|||) --foldr ((|||) . draw') mempty


-- The origins aren't really in the right place.....
-- They don't match where the origins of the orginal parent tiles are
--Translation should always be before rotation in the composition otherwise you get weird results 
-- TODO t might have to be last in the composition of transformations, not first
-- doesn't deflate correctly, tried t first last and not at all. none work
-- Since t is a path from the shape to the origin, maybe I need to transform by the inverse of t
deflateTile :: Tile Penrose -> [Tile Penrose]
deflateTile (Tile Kite t) = [Tile HalfKite t, Tile HalfKite (t <> reflectionY)] -- origin matches parent tile origin
deflateTile (Tile Dart t) = [Tile HalfDart t, Tile HalfDart (t <> translation (e (-36 @@ deg)) <> rotation (-108 @@ deg))] -- matches
deflateTile (Tile HalfKite t) = [Tile Kite (t <> reflectionX <> translation (-e (0 @@ deg)) <> rotation (-72 @@ deg) <> translation (-e (0 @@ deg))), Tile HalfDart (t <> reflectionX <> translation (-e (0 @@ deg)) <> rotation (-72 + 36 @@ deg) <> reflectionY)] --matches
deflateTile (Tile HalfDart t) = [Tile HalfDart (t <> translation (e (-72 @@ deg)) <> rotation (144 @@ deg)), Tile HalfKite (t <> translation (e (-72 @@ deg)) <> reflectionX <> translation (-e (0 @@ deg)))] --matches

deflateTile' :: Tile Penrose -> [Tile Penrose]
deflateTile' (Tile Kite t) = [Tile HalfKite mempty, Tile HalfKite (reflectionY)] -- origin matches parent tile origin
deflateTile' (Tile Dart t) = [Tile HalfDart mempty , Tile HalfDart (reflectionY)] -- matches
deflateTile' (Tile HalfKite t) = [Tile Kite (reflectionX <> rotation (-72 @@ deg)), Tile HalfDart (reflectionX <> reflectionY <>rotation (36 @@ deg))] --matches
deflateTile' (Tile HalfDart t) = [Tile HalfDart ( rotation (144 @@ deg)), Tile HalfKite (reflectionX)] --matches


--draw :: Tile a -> Diagram B
--draw = mkTiling . vertices

-- instance Functor Tile where
--     fmap f (tile)

-- instance Semigroup (Tile a) where
--  (<>) t1 t2 = Tile const $ edges t1 <> edges t2


type MatchingRule a = [Tile a] -> QDiagram B V2 Double Any
data Tile' a = Tile' a (MatchingRule a)

-- data Pattern a = Patten {
--     curTiles :: Tile a => [a],
--     follow :: Int -> Pattern a
-- }

--tileTest2 = renderSVG "images/tTest.svg" (mkWidth 400) $ deflate HalfKite

--Draws each list beside each other. Draws each element inside a nested list on top of each other
draw'' :: [[Tile Penrose]] -> QDiagram B V2 Double Any
draw'' = foldr ((|||) . foldr ((<>) . draw') mempty) mempty

tiles = [Tile Kite mempty, Tile Dart mempty, Tile HalfKite mempty, Tile HalfDart mempty]
--tileTest2 = renderSVG "images/tTest.svg" (mkWidth 400) $ foldr ((|||) . draw') mempty tiles === draw'' (map deflateTile tiles)
--tileTest2 = renderSVG "images/tTest.svg" (mkWidth 400) $ foldr ((<>) . draw') mempty (deflateTile (Tile HalfDart mempty))

--tileTest2 = renderSVG "images/tTest.svg" (mkWidth 400) $ deflateRec [Tile Dart mempty] 2
tileTest2 = renderSVG "images/tTest.svg" (mkWidth 400) $ foldr ((|||). deflateNdraw 2) mempty tiles

--guiDemo :: Penrose -> Int -> Text
guiDemo t z = renderText $ renderDia SVG (SVGOptions (mkWidth 600) Nothing "" [] True) $ deflateNdraw z t

deflateRec :: [Tile Penrose] -> Int -> QDiagram B V2 Double Any
deflateRec t 0 = foldr ((<>) . draw') mempty t
deflateRec t n = deflateRec (concatMap deflateTile t) (n-1)
