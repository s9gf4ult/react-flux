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

transformSelect :: Transform SelectAction SelectState
transformSelect action state = case action of
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
selectDispatch a = [mkSomeStoreAction transformSelect selectStore a]
