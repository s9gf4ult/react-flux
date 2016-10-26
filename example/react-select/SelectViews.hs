{-# LANGUAGE OverloadedStrings, BangPatterns, TemplateHaskell #-}

module SelectViews where

import Control.Concurrent
import Data.Aeson
import Data.Aeson.TH
import Data.Char
import Data.Monoid
import Data.Text (Text)
import GHCJS.Marshal.Internal
import React.Flux
import SelectStore

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

-- | Posible values to select from
options :: [SelectOption Int]
options = map toOption [0..100]
  where
    toOption o = SelectOption o $ T.pack $ (show o) ++ " characters"

selectApp :: ReactView ()
selectApp = defineControllerView "select app" selectStore $ \state () -> do
  div_ $ do
    foreign_ ["Select"]
      [ callbackReturning "filterOptions" filterOptions
      , callback "onChange" $ \so -> selectDispatch $ SelectValue $ soValue so
      , "options" @= options
      , "value" @= (ssValue state) ]
      mempty

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
