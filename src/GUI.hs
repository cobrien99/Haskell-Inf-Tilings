module GUI (gui) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Reactive.Threepenny

-- This is the GUI for the program
-- It's written in an FRP style
--  Events include: changing zoom level and scrolling, possibly auto tile plane button, selecting tile set
--  Behaviours would be: The image displayed, current tile set
-- Contains a list on the side with your tile sets
-- and currently buttons to control events
-- Most of the screen displays diagram image

gui :: IO ()
gui = startGUI defaultConfig main


main :: Window -> UI ()
main w = do
    return w # set title "GUI title"

    image <- string "Imagine an image"
    --The buttons
    zoomIn <- UI.button #+ [string "+"]
    zoomOut <- UI.button #+ [string "-"]

    ts1 <- UI.button #+ [string "Penrose"] --in the future I want these tiles to be programmatically generated based on supplied tses

    t1 <- UI.button #+ [string "Skinny"]
    t2 <- UI.button #+ [string "Fat"]

    verticalLine <- mkElement "vline" # set style [("border-left","thick solid #ff0000")]

    tileController <- mkElement "tileController" #+ [row [element verticalLine, column [string "Select TileSet", element ts1]]]

    -- events --
    let
        --Takes an event, turns it into "increase zoom event"
        eZoomIn :: Event (Int -> Int)
        eZoomIn = (+1) <$ UI.click zoomIn

        --Like above but "decrease zoom event"
        --By convention zoom can't go below 1
        eZoomOut :: Event (Int -> Int)
        eZoomOut = (\z -> max 0 (z-1)) <$ UI.click zoomOut

        zoomChangeEvent = unionWith const eZoomIn eZoomOut

        --needs to know what TS it selected
        --atm I just return a string corresponding to a TS but obvs this ain't great
        -- Could replace it with TileSet -> TileSet once I build those
        -- Need to change this to work on a list of ts, extract its name
        eSelectTS :: Event (String -> String)
        eSelectTS = const "Penrose" <$ UI.click ts1

        eSelectT1 :: Event (String -> String)
        eSelectT1 = const "Skinny" <$ UI.click t1


        eSelectT2 :: Event (String -> String)
        eSelectT2 = const "Fat" <$ UI.click t2

        -- fires when a tile is selected
        -- atm just fires when the dummy buttons fires
        eSelectAllT :: Event (String -> String)
        eSelectAllT = unionWith const eSelectT1 eSelectT2






    --behaviour that tracks zoom over time
    zoomLevel <- accumB 1 zoomChangeEvent

    currentTS <- accumB "" eSelectTS

    currentT <- accumB "" eSelectAllT



    let
        --event that fires whenever zoom button clicked, tells program to update drawing with current zoom val
        eNewZoom :: Event Int
        eNewZoom = zoomLevel <@ zoomChangeEvent

        -- could I replace this with eSelectTS :: Event (String)
        eChangedTS :: Event String
        eChangedTS = currentTS <@ eSelectTS

        eChangedT :: Event String
        eChangedT = currentT <@ eSelectAllT

    -- onEvent eNewZoom $ \e -> do 
    --     newZC <- string $ show e
    --     set children [newZC] (element zoomCounter)


    -- zoomCounter <- sink UI.content zoomLevel (string "1")


    --The page layout
    getBody w #+ [row [column [element image, row [element zoomIn, element zoomOut, element zoomCounter]], element tileController]]
  
      











    return ()