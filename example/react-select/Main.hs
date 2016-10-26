{-# LANGUAGE OverloadedStrings #-}

module Main where

import GHCJS.Types
import React.Flux
import React.Flux.Combinators
import SelectViews

main :: IO ()
main = reactRender "selectApp" selectApp ()
