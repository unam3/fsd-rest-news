{-# LANGUAGE DeriveGeneric      #-}

module AesonDefinitions (
    CreateUserRequest(..),
    GetUserRequest(..)
    ) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), decode, defaultOptions, fieldLabelModifier, genericParseJSON, genericToJSON)
import Data.Int (Int16)
import Data.Text (Text)
import GHC.Generics (Generic)

data CreateUserRequest = CreateUserRequest {
    name :: Text,
    surname :: Text,
    is_admin :: Bool
} deriving (Show, Generic)

instance FromJSON CreateUserRequest

data GetUserRequest = GetUserRequest {
    user_id :: Int16
} deriving (Show, Generic)

instance FromJSON GetUserRequest
