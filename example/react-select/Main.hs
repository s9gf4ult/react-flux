module Main where

import React.Flux
import SelectViews

main :: IO ()
main = reactRender "selectApp" selectApp ()
