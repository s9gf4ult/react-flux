{-# LANGUAGE OverloadedStrings, BangPatterns, TemplateHaskell #-}

module SelectViews where

import Data.Aeson
import Data.Aeson.TH
import Data.Char
import Data.Text (Text)
import GHCJS.Marshal.Internal
import React.Flux
import SelectStore

-- import qualified Data.Text as T

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
      mempty

filterOptions :: [SelectOption Text] -> String -> [a] -> IO [SelectOption Text]
filterOptions = error "FIXME: not implemented"
