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
  { seValue          :: !a
  , seLabel          :: !Text
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

selectApp :: ReactView ()
selectApp = defineControllerView "select app" selectStore $ \state () -> do
  div_ $ do
    foreign_ "Select"
      [callbackReturning3 "filterOptions" filterOptions]
      -- []
      mempty

-- | Callback generating list of select options
filterOptions
  :: Maybe [SelectOption Text]
  -> Maybe String
  -> Maybe [Text]
  -> IO [SelectOption Text]
filterOptions _ (Just str) _ = do
  print str
  -- threadDelay 1000 -- < with this line we get error
  return $ map toOpt opts
  where
    toOpt t = SelectOption t t
    opts = map (T.pack . (str <>) . show) [1..5]
filterOptions _ _ _ = do
  print "no str given"
  return []
