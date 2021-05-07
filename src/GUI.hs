{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-} --have to use recursive do so we can refer to events before we define them
{-# LANGUAGE ScopedTypeVariables #-}
module GUI (gmain) where

import Reflex.Dom
import Data.Text.Lazy ( toStrict ) --there could be some way to leverage this laziness
import Draw (filepath, draw)
import Tile2 (kiteCons, dartCons, tile, guiDemoTiling, single, inflateTiling)

gmain :: IO ()
gmain = mainWidget deflateDemo

serpDemo :: MonadWidget t m => m ()
serpDemo = do
   el "h2" $ text "Control the zoom"
   rec
       dynNum <- foldDyn ($) (1 :: Int) $ leftmost [(+1) <$ evIncr, (\x -> max 1 (x-1)) <$ evDecr, const 1 <$ evReset]
       let dynImage =  toStrict . draw <$> dynNum --Thread this
       elDynHtml' "div" dynImage
       el "div" $ display dynNum
       evIncr <- button "Zoom Out"
       evDecr <- button "Zoom In"
       evReset <- button "Reset Zoom"
   return ()

infiniteTiling = guiDemoTiling

deflateDemo :: MonadWidget t m => m ()
deflateDemo = tilingGUI

tilingGUI :: MonadWidget t m => m ()
tilingGUI = do
    rec -- Allows reference of events before they are defined

        -- Depth and root tile signals 
        dynDepth <- foldDyn ($) (0 :: Int) $ 
            leftmost [(+1) <$ evIncr,
                      (\x -> max 0 (x-1)) <$ evDecr,
                      const 0 <$ evReset]
        dynRootTile <- foldDyn ($) (tile kiteCons mempty) $ 
            leftmost [  const (tile kiteCons mempty) <$ evKite, 
                        const (tile dartCons mempty) <$ evDart]
        -- Combining signals to produce & display the tiling image
        let dynImage =  toStrict <$> (infiniteTiling <$> dynRootTile <*> dynDepth)
        elDynHtml' "div" dynImage
        --State info
        el "div" $ do 
            text "Root: "
            display dynRootTile
            text ", Depth: "
            display dynDepth
            text ", Tiles: "
            display ((2 ^) <$> dynDepth)
            text ", Nodes: "
            display ((\x -> (2^(x+1))-1) <$> dynDepth)
        -- Tile selection buttons
        evKite <- button "Kite"
        evDart <- button "Dart"
        el "div" (return ())
        -- Depth control buttons
        evIncr <- button "Increase Depth"
        evDecr <- button "Decrease Depth"
        evReset <- button "Reset Depth"
    return ()

-- how does (display =<< count =<< button "ClickMe") work?
    -- button creates an event
    -- count creates a dynamic with the total number of times event fires
    -- (dynamic is like a behaviour that notifies when its changed, halfway between event and behaviour)
    -- display creates a DOM element with a string representation of this number and displays it as DOM element.
