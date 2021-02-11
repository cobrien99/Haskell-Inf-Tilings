module Main where

import GUI ( gmain )
import Draw ( draw )
import Girih ( gTest )
import Tile (tileTest)

main :: IO ()
main = do
    tileTest
