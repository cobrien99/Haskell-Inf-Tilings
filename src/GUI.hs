{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-} --have to use recursive do so we can refer to events before we define them
module GUI (gmain) where

import Reflex.Dom
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Monoid
import Data.FileEmbed
import Draw (filepath, draw)

gmain :: IO ()
gmain = mainWidget bodyElement

bodyElement :: MonadWidget t m => m ()
bodyElement = do
   el "h2" $ text "Control the zoom"
   rec dynNum <- foldDyn ($) (1 :: Int) $ leftmost [(+1) <$ evIncr, (+ (-1)) <$ evDecr, const 1 <$ evReset]
       let dynAttrs = attrs <$> dynNum
       el "div" $ display dynNum
       evIncr <- button "Zoom In"
       evDecr <- button "Zoom Out"
       evReset <- button "Reset Zoom"
       elDynAttr "img" dynAttrs blank
   return ()


attrs :: Int -> Map.Map T.Text T.Text
attrs z = "style" =: ("src: " <> "output.svg" )

imgAttrs :: Map.Map T.Text T.Text
imgAttrs = "src" =: "output.svg"--"https://www.w3schools.com/images/lamp.jpg"--"images/cat.jpg"--T.pack filepath


-- how does (display =<< count =<< button "ClickMe") work?
    -- button creates an event
    -- count creates a dynamic with the total number of times event fires
    -- (dynamic is like a behaviour that notifies when its changed, halfway between event and behaviour)
    -- display creates a DOM element with a string representation of this number and displays it as DOM element.
