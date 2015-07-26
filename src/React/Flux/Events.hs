module React.Flux.Events (
    EventHandler
  , Event(..)

  -- * Keyboard
  , KeyboardEvent(..)
  , onKeyDown
  , onKeyPress
  , onKeyUp
) where

import Data.Aeson
import React.Flux.JsTypes

-- | TODO
data EventHandler handler = EventHandler
  { evtHandlerName :: String
  , evtHandler :: RawEvent -> handler
  }

instance Functor EventHandler where
    fmap f (EventHandler name h) = EventHandler name (f . h)

----------------------------------------------------------------------------------------------------
--- Generic Event
----------------------------------------------------------------------------------------------------

data Event = Event
    { evtType :: String
    , evtBubbles :: Bool
    , evtCancelable :: Bool
    -- evtCurrentTarget
    , evtDefaultPrevented :: Bool
    , evtPhase :: Int
    , evtIsTrusted :: Bool
    -- evtNativeEvent
    -- evtTarget
    , evtTimestamp :: Int
    }

instance FromJSON Event where
    parseJSON = withObject "Event" $ \o -> do
        Event <$> o .: "type"
              <*> o .: "bubbles"
              <*> o .: "cancelable"
              <*> o .: "defaultPrevented"
              <*> o .: "eventPhase"
              <*> o .: "isTrusted"
              <*> o .: "timestamp"

{-
foreign import javascript unsafe
    "$1.preventDefault();"
    js_preventDefault :: JSRef RawEvent_ -> IO ()

preventDefault :: RawEvent -> IO ()
preventDefault (RawEvent ref _) = js_preventDefault ref

foreign import javascript unsafe
    "$1.stopPropagation();"
    js_stopProp :: JSRef RawEvent_ -> IO ()

stopPropagation :: RawEvent -> IO ()
stopPropagation (RawEvent ref _) = js_stopProp ref
-}

parseEvent :: RawEvent -> Event
parseEvent (RawEvent _ val) =
    case fromJSON val of
        Error err -> error $ "Unable to parse event: " ++ err
        Success e -> e

mkHandler :: String
          -> (RawEvent -> detail)
          -> (Event -> detail -> handler)
          -> EventHandler handler
mkHandler name parseDetail f = EventHandler
    { evtHandlerName = name
    , evtHandler = \raw -> f (parseEvent raw) (parseDetail raw)
    }

---------------------------------------------------------------------------------------------------
--- Keyboard
---------------------------------------------------------------------------------------------------

data KeyboardEvent = KeyboardEvent
  { keyEvtAltKey :: Bool
  , keyEvtCharCode :: Int
  , keyEvtCtrlKey :: Bool
  , keyGetModifierState :: String -> Bool
  , keyKey :: String
  , keyCode :: Int
  , keyLocale :: String
  , keyLocation :: Int
  , keyMetaKey :: Bool
  , keyRepeat :: Bool
  , keyShiftKey :: Bool
  , keyWhich :: Int
  }

instance FromJSON KeyboardEvent where
    parseJSON = withObject "Keyboard Event" $ \o ->
        KeyboardEvent <$> o .: "altKey"
                      <*> o .: "charCode"
                      <*> o .: "ctrlKey"
                      <*> return (pure False) -- this is set in 'parseKeyboardEvent'
                      <*> o .: "key"
                      <*> o .: "keyCode"
                      <*> o .: "locale"
                      <*> o .: "location"
                      <*> o .: "metaKey"
                      <*> o .: "repeat"
                      <*> o .: "shiftKey"
                      <*> o .: "which"

#ifdef __GHCJS__
foreign import javascript unsafe
    "$r = $1.getModifierState($2)"
    js_GetModifierState :: JSRef RawEvent_ -> JSString -> JSBool

getModifierState :: JSRef RawEvent_ -> String -> Bool
getModifierState ref = pFromJSRef . js_GetModifierState . pToJSRef
#else
getModifierState :: JSRef RawEvent_ -> String -> Bool
getModifierState _ _ = False
#endif

parseKeyboardEvent :: RawEvent -> KeyboardEvent
parseKeyboardEvent (RawEvent ref val) =
    case fromJSON val of
        Error err -> error $ "Unable to parse keyboard event: " ++ err
        Success e -> e
                { keyGetModifierState = getModifierState ref
                }

onKeyDown :: (Event -> KeyboardEvent -> handler) -> EventHandler handler
onKeyDown = mkHandler "onKeyDown" parseKeyboardEvent

onKeyPress :: (Event -> KeyboardEvent -> handler) -> EventHandler handler
onKeyPress = mkHandler "onKeyPress" parseKeyboardEvent

onKeyUp :: (Event -> KeyboardEvent -> handler) -> EventHandler handler
onKeyUp = mkHandler "onKeyUp" parseKeyboardEvent
