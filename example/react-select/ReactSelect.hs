{-# LANGUAGE TypeFamilies, DeriveGeneric, OverloadedStrings
  , TemplateHaskell, GeneralizedNewtypeDeriving, JavaScriptFFI #-}


module ReactSelect where

import Control.DeepSeq
import Data.Aeson
import Data.Aeson.TH
import Data.Char
import Data.Text (Text)
import GHCJS.Marshal
import GHCJS.Types
import React.Flux

import qualified Data.Text as T

data SelectOption a = SelectOption
  { soValue          :: !a
  , soLabel          :: !Text
  } deriving (Show)

deriveJSON
  defaultOptions
  { fieldLabelModifier = map toLower . drop 2 }
  ''SelectOption

instance (FromJSON a) => FromJSVal (SelectOption a) where
  fromJSVal j = do
    Just value <- fromJSVal j
    return $ case fromJSON value of
      Error _ -> Nothing
      Success a -> Just a

instance (ToJSON a) => ToJSVal (SelectOption a) where
  toJSVal = toJSVal . toJSON

newtype LoadCallback a
  = LoadCallback JSVal
  deriving (ToJSVal, FromJSVal, NFData)

foreign import javascript unsafe
  "$1(null, {options: $2, complete: true})"
  js_callLoadCallback :: JSVal -> JSVal -> IO ()

callLoadCallback :: (ToJSON a) => LoadCallback a -> [SelectOption a] -> IO ()
callLoadCallback cb so = do
  cbval <- toJSVal cb
  soval <- toJSVal so
  js_callLoadCallback cbval soval

-- | Posible values to select from
options :: [SelectOption Int]
options = map toOption [0..100]
  where
    toOption o = SelectOption o $ T.pack $ (show o) ++ " characters"

-- | Callback generating list of select options
filterOptions
  :: Maybe [SelectOption Int]   -- ^ options from element
  -> Maybe String               -- ^ string user just entered
  -> Maybe [Int]                -- ^ options user selected
  -> IO [SelectOption Int]      -- ^ options to show to the user
filterOptions (Just opts) (Just str) _ = do
  print str
  -- threadDelay 1000 -- < with this line we get error, be carefull
  return $ filter lengthAround opts
  where
    lengthAround op =
      let val = soValue op
          len = length str
      in val >= (len - 5) && val <= (len + 5)

filterOptions _ _ _ = do
  print "wrong arguments given"
  return []
