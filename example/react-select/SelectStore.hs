{-# LANGUAGE TypeFamilies, DeriveGeneric, OverloadedStrings #-}

module SelectStore where

import Control.DeepSeq
import Data.Text as T
import GHC.Generics (Generic)
import React.Flux

data SelectState = SelectState
  { ssValue :: Int
  }

data SelectAction
  = SelectValue Int
  deriving (Generic)

instance NFData SelectAction

instance StoreData SelectState where
  type StoreAction SelectState = SelectAction
  transform action state = case action of
    SelectValue v -> do
      print v
      return $ SelectState v

selectStore :: ReactStore SelectState
selectStore = mkStore $ SelectState 0

selectDispatch :: SelectAction -> ViewEventHandler
selectDispatch a = [SomeStoreAction selectStore a]
