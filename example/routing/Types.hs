{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}

-- |

module Types where

import           React.Flux


import qualified Data.Text     as T
import           Data.Typeable (Typeable)
import qualified Data.JSString.Text as JSS

type AppName = T.Text
type AppView = ReactElementM ViewEventHandler
type AppRouter = [T.Text] -> IO ()

data App props = forall state action. (Typeable state) =>
           App {appName        :: AppName
               , appTransform  :: Transform action state
               , appState      :: ReactStore state
               , appView       :: Typeable props => state -> props -> AppView ()
               , appInitAction :: action
               , appRouter     :: Maybe AppRouter
               }
               deriving Typeable

initApp :: Typeable props => App props -> IO (ReactView props)
initApp App{..} = do
  let view' = defineControllerView (JSS.textToJSString appName) appTransform appState (\st props -> appView st props)
  alterStore appTransform appState appInitAction
  return view'
