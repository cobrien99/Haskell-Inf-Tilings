module Main where

import GUI ( gmain )
import Draw ( draw )

main :: IO ()
main = do
    draw 1
    gmain
