{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-} --have to use recursive do so we can refer to events before we define them
{-# LANGUAGE ScopedTypeVariables #-}
module GUI (gmain) where

import Reflex.Dom
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Monoid
import Data.FileEmbed
import Data.Text.Lazy ( toStrict ) --there could be some way to leverage this laziness
import Draw (filepath, draw)
import Control.Monad ((<=<), void)
import Data.Maybe (listToMaybe)
import GHCJS.DOM.EventM (on)
import GHCJS.DOM.FileReader (newFileReader, readAsDataURL, load, getResult)

gmain :: IO ()
gmain = mainWidget bodyElement

-- bodyElement :: MonadWidget t m => m ()
-- bodyElement = do
--    el "h2" $ text "Control the zoom"
--    rec dynNum <- foldDyn ($) (1 :: Int) $ leftmost [(+1) <$ evIncr, (+ (-1)) <$ evDecr, const 1 <$ evReset]
--        let dynAttrs = attrs <$> dynNum
--        el "div" $ display dynNum
--        evIncr <- button "Zoom In"
--        evDecr <- button "Zoom Out"
--        evReset <- button "Reset Zoom"
--        elDynAttr "img" dynAttrs blank
--    return ()

bodyElement :: MonadWidget t m => m ()
bodyElement = do
   el "h2" $ text "Control the zoom"
   rec 
       dynNum <- foldDyn ($) (1 :: Int) $ leftmost [(+1) <$ evIncr, (\x -> max 1 (x-1)) <$ evDecr, const 1 <$ evReset]
       let dynImage =  toStrict . draw <$> dynNum --ok wow I HAVE to thread this it's pretty slow otherwise
       elDynHtml' "div" dynImage
       el "div" $ display dynNum
       evIncr <- button "Zoom In"
       evDecr <- button "Zoom Out"
       evReset <- button "Reset Zoom"
    --    let
--     --        srcAttr :: Event t (M.Map T.Text T.Text)= ffor (return filepath ) $ \u -> "src" =: u
--        srcAttrDyn :: Dynamic t (M.Map T.Text T.Text) <- holdDyn mempty (ffor (constDyn $ T.pack filepath ) $ \u -> "src" =: u)
   return ()

-- how does (display =<< count =<< button "ClickMe") work?
    -- button creates an event
    -- count creates a dynamic with the total number of times event fires
    -- (dynamic is like a behaviour that notifies when its changed, halfway between event and behaviour)
    -- display creates a DOM element with a string representation of this number and displays it as DOM element.
