{-# LANGUAGE OverloadedStrings, BangPatterns, TemplateHaskell #-}

module SelectViews where

import Data.Aeson.TH
import Data.Char
import Data.Monoid
import Data.Text (Text)
import GHCJS.Marshal.Internal
import GHCJS.Types
import React.Flux
import ReactSelect
import SelectStore

import qualified Data.Text as T

selectApp :: ReactView ()
selectApp = defineControllerView "select app" selectStore $ \state () -> do
  div_ $ do
    foreign_ ["Select"]
      [ callbackReturning "filterOptions" filterOptions
      , callback "onChange" $ \so -> selectDispatch $ SelectValue $ soValue so
      , "options" @= options
      , "value" @= (ssValue state) ]
      mempty
    foreign_ ["Select", "Async"]
      [ callback "loadOptions" loadOptions ]
      mempty

loadOptions :: String -> LoadCallback Int -> ViewEventHandler
loadOptions str cb = selectDispatch $ LoadOptions str cb
