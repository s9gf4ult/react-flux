{-# LANGUAGE TypeFamilies, DeriveGeneric, OverloadedStrings #-}

module SelectStore where

import Control.DeepSeq
import GHC.Generics (Generic)
import React.Flux
import ReactSelect

data SelectState = SelectState
  { ssValue :: Int
  }

data SelectAction
  = SelectValue Int
  | LoadOptions String (LoadCallback Int)
  deriving (Generic)

instance NFData SelectAction

instance StoreData SelectState where
  type StoreAction SelectState = SelectAction
  transform action state = case action of
    SelectValue v -> do
      print v
      return $ SelectState v
    LoadOptions str cb -> do
      print str
      opts <- filterOptions (Just options) (Just str) Nothing
      callLoadCallback cb opts
      return state

selectStore :: ReactStore SelectState
selectStore = mkStore $ SelectState 0

selectDispatch :: SelectAction -> ViewEventHandler
selectDispatch a = [SomeStoreAction selectStore a]
