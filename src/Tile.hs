{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Tile (tileTest) where

import Diagrams.Prelude hiding (dart, halfDart, dart', halfDart')
import Diagrams.Backend.SVG
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Vector ( e )

--Maybe I should use the Dias equivalent
--could vertex be a map?
-- then I could just work with a list of points instead of pettern matching inside the tuples all the time
-- I only need the Bools twice so far after all
type Vertex = (Point V2 Double, Bool)
type Edge = (Vertex, Vertex)

--converts an edge to a vector pointing from the first vertex to the 2nd vertex
eToV :: Edge -> V2 Double
eToV ((p1, c1), (p2, c2)) = p1 .-. p2

--returns the centroid of an edge, the halfway point of that edge
eToC :: Edge -> Point V2 Double
eToC ((p1, _), (p2, _)) = centroid [p1, p2]

--eToL :: Edge -> Line 
eToL ((p1, c1), (p2, c2)) = p1 ~~ p2


--What if they would match when flipped?
doesMatch :: Edge -> Edge -> Bool
doesMatch ((p1, c1), (p2, c2)) ((p3, c3), (p4, c4)) | not $ (c1 == c3) && (c2 == c4) = False --check the colours at the vertices match
                                                    | norm (p1 .-. p3) /= norm (p2 .-. p4) = False --Check the lengths of the edges are the same
                                                    | otherwise = True
                                                    --direction?? probably not

doesMatch' :: (Edge, Edge) -> Bool
doesMatch' (e1, e2) = doesMatch e1 e2

--What to do with a singleton list? return empty list, there is no edges after all
--putting last vs on the start hould be the same as putting head vs on end. Possibly not clear and should change tho
vToE :: [Vertex] -> [Edge]
vToE vs = if head vs /= last vs then f (last vs: vs) else []
    where   f [v] = []
            f (v:vs) = (v, head vs) : vToE vs


tToE :: Tile a => a -> [Edge]
tToE = vToE . vertices

--takes two tiles a and b
--destructs them into a list of their edges
--checks every edge in a aganist every edge in b
--if a match is found the resulting tuple is returned
-- matchingEdges ::(Tile a b) => a -> b -> [(Edge, Edge)]
-- matchingEdges t1 t2 =

--Generates all combinations of (a,b) ,no (b,a)
--filters then using doesMatch'
matchingEdges ::[Edge] -> [Edge] -> [(Edge, Edge)]
matchingEdges a b =  doesMatch' `filter` allEdges a b
    where   allEdges [] _ = []
            allEdges (a:as) b = map (a, ) b ++ matchingEdges as b

--I mean this surely could be a functor
matchingEdges' a b = matchingEdges (tToE a) (tToE b)
                                                    
--checks if two vectors match and returns a transformation to apply to the second edge to get it to line up with the first
--technically transformation could include scale but it should just include rotation
--what about transformation
--rotation and translation order could be wrong. translation operator could be wrong too
match :: Edge -> Edge -> Transformation V2 Double
match e1 e2 = if doesMatch e1 e2 then translation (c1 .-. c2) <> rotation (angleBetween v1 v2)  else mempty
            where
                v1 = eToV e1
                v2 = eToV e2
                c1 = eToC e1
                c2 = eToC e2
 --and the distance between points is a match factor



class Tile a where
    vertices :: a -> [Vertex]

    draw :: a -> Diagram B
    draw = mkTiling . vertices

mkTiling :: [Vertex] -> QDiagram B V2 Double Any
mkTiling vertices = body <> translate (origin .-. centroid ps) (matchingRules ps mrs)
  where
    (ps, mrs) = unzip vertices
    body = translate (head ps .-. centroid ps) $ strokeLine $ fromVertices (ps ++ [head ps]) --from vertices sets the first point as the origin so have to correct for that
    matchingRules (b:bs) (p:ps)= mr b p <> matchingRules bs ps --matching rule treats (0,0) a orgin so have to correct for that too
    matchingRules [] [] = mempty

mr :: Point V2 Double -> Bool -> QDiagram B V2 Double Any
mr p True  = circle 0.05 # fc black # moveTo p 
mr p False = circle 0.05 # moveTo p 

-- Maybe deflate is not the best term for this, what about matching rules?
-- should there be a constraint on a to force it to be a tile? Might have to use GADTS so
data Tiling a = Base (Transformation V2 Double) a | Deflate [Tiling a]

draw' :: Tile a => Tiling a -> Diagram B
draw' (Base t a) = transform t $ draw a
draw' (Deflate ts) = mconcat $ map (draw') ts

instance Functor Tiling where
    fmap f (Base t a) = Base t (f a)
    fmap f (Deflate t) = Deflate (map (fmap f) t)

-- instance Semigroup (Tiling a) where
--     t1 <> t2 = Deflate [t1, t2] -- pretty sure this doesn't satisy a law [x <> (y <> z) = (x <> y) <> z]

-- instance Monoid (Tiling a) where




--class (Tile a) => Tiling a where 
-- Tile set is where the deflation/matching rules are defined
class (Tile a) => Tileset a where
    deflate :: Tiling a -> Tiling a
    --deflate (Deflate ts) = concat $ map deflate ts
    --match :: Tiling a -> Tiling a -> Tiling a


--Can I make tilings an instance of Monoid, Semigroup, Foldable?

--------------------------------------------

data Penrose = Kite | Dart | HalfKite | HalfDart



--some points that are import
pr0 :: (Additive f, Num a) => Point f a
pr0 = origin 
pr1 :: (Additive f, HasR f, RealFloat n, HasTheta f) => Point f n
pr1 = origin & _r .~ 1 & _theta .~ (36 @@ deg)
pr2 :: (Additive f, HasR f, RealFloat n, HasTheta f) => Point f n
pr2 = origin & _r .~ 1 & _theta .~ (0 @@ deg)
pr3 :: (Additive f, HasR f, RealFloat n, HasTheta f) => Point f n
pr3 = origin & _r .~ 1 & _theta .~ (-36 @@ deg)
pr4 :: RealFloat a => Point V2 a
pr4 = pr1 .+^ e (-36 @@ deg) --could write this as p1 .~ 1 & _theta .~ (-36 @@ deg)



instance Tile Penrose where
  vertices Kite =     [pr0, pr1, pr2, pr3] `zip` [True, False, True, False]
  vertices Dart =     [pr2, pr3, pr4, pr1] `zip` [False, True, False, True]
  vertices HalfKite = [pr0, pr1, pr2]      `zip` [True, False, True]
  vertices HalfDart = [pr2, pr4, pr1]      `zip` [False, False, True]

-- so much boilerplate here
instance Tileset Penrose where
    deflate (Base t Kite) = Deflate [Base t HalfKite, Base (t <> reflectionY) HalfKite]
    deflate (Base t Dart) = Deflate [Base t HalfDart, Base (t <> reflectionY) HalfDart]
    deflate (Base t HalfKite) = Deflate [Base (t <> rotation (-144 @@ deg)) HalfDart, Base (t <> rotation (-108 @@ deg)) Kite]
    deflate (Base t HalfDart) = Deflate [Base (t <> rotation (180-36 @@ deg)) HalfDart, Base (t <> reflectionX) HalfKite]




baseTiles = map (Base mempty) [Kite, Dart, HalfDart, HalfKite]
deflations = map deflate baseTiles
--tileTest = renderSVG "images/tTest.svg" (mkWidth 400) $ foldr ((|||) . showOrigin . draw') mempty  $ baseTiles  ++ deflations

tileTest = renderSVG "images/tTest.svg" (mkWidth 400) $ foldr ((|||) . showOrigin . draw') mempty  $ [Base mempty HalfDart, Base t Kite]
    where 
        matches= matchingEdges' HalfDart Kite
        t = case matches of 
            ((e1,e2):ms) -> match e1 e2
            _            -> mempty 
        
