{-# LANGUAGE OverloadedStrings #-}
module GUI (gmain) where

import Reflex.Dom
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Monoid

gmain :: IO ()
gmain = mainWidget $ display =<< count =<< button "ClickMe"