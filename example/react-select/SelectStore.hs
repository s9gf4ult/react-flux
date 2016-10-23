{-# LANGUAGE TypeFamilies, DeriveGeneric, OverloadedStrings #-}

module SelectStore where

import Control.DeepSeq
import GHC.Generics (Generic)
import React.Flux

data SelectState = SelectState

data SelectAction = SelectAction
                  deriving (Generic)

instance NFData SelectAction

instance StoreData SelectState where
  type StoreAction SelectState = SelectAction
  transform action state = return state


selectStore :: ReactStore SelectState
selectStore = mkStore SelectState

selectDispatch :: SelectAction -> ViewEventHandler
selectDispatch a = [SomeStoreAction selectStore a]
