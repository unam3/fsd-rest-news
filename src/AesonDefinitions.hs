{-# LANGUAGE DeriveGeneric      #-}

module AesonDefinitions (
    CreateUserRequest(..)
    ) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), decode, defaultOptions, fieldLabelModifier, genericParseJSON, genericToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data CreateUserRequest = CreateUserRequest {
    name :: Text,
    surname :: Text,
    is_admin :: Bool
} deriving (Show, Generic)

--instance ToJSON CreateUserRequest
instance FromJSON CreateUserRequest
