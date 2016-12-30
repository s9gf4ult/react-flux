module Dispatcher (
    -- re-export NavPageId so views don't have to import NavStore directly
    NavPageId(..)
  , dispatch
) where

import React.Flux
import NavStore

dispatch :: NavAction -> [SomeStoreAction]
dispatch action =
  [mkSomeStoreAction transformNavState currentNavPageStore action]
