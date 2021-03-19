{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-} --have to use recursive do so we can refer to events before we define them
{-# LANGUAGE ScopedTypeVariables #-}
module GUI (gmain) where

import Reflex.Dom
import Data.Text.Lazy ( toStrict ) --there could be some way to leverage this laziness
import Draw (filepath, draw)
import Tile2 (kiteCons, dartCons, tile, guiDemoTiling, deflateTiling, single, inflateTiling)

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

deflateDemo :: MonadWidget t m => m ()
deflateDemo = do
    rec
        dynZoom <- foldDyn ($) (single $ tile kiteCons mempty) $ leftmost [deflateTiling <$ evIncr, inflateTiling <$ evDecr]
        dynTiling <- foldDyn ($) (single $ tile kiteCons mempty) $ leftmost [const (single $ tile kiteCons mempty) <$ evKite, const (single $ tile dartCons mempty) <$ evDart]
        let dynImage =  toStrict <$> ((guiDemoTiling <$> dynZoom))
        elDynHtml' "div" dynImage
        evKite <- button "Kite"
        evDart <- button "Dart"
        --el "div" $ display dynZoom
        evIncr <- button "Zoom Out"
        evDecr <- button "Zoom In"
        --revReset <- button "Reset Zoom"
    return ()

-- how does (display =<< count =<< button "ClickMe") work?
    -- button creates an event
    -- count creates a dynamic with the total number of times event fires
    -- (dynamic is like a behaviour that notifies when its changed, halfway between event and behaviour)
    -- display creates a DOM element with a string representation of this number and displays it as DOM element.
