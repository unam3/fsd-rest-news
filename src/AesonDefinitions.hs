{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE DeriveGeneric      #-}

module AesonDefinitions (
    CreateUserRequest(..),
    UserIdRequest(..),
    CreateTagRequest(..),
    EditTagRequest(..),
    TagIdRequest(..)
    ) where

import Data.Aeson (FromJSON)
import Data.Int (Int16)
import Data.Text (Text)
import GHC.Generics (Generic)

data CreateUserRequest = CreateUserRequest {
    name :: Text,
    surname :: Text,
    avatar :: Text,
    is_admin :: Bool
} deriving (Show, Generic)

instance FromJSON CreateUserRequest


data UserIdRequest = UserIdRequest {
    user_id :: Int16
} deriving (Show, Generic)

instance FromJSON UserIdRequest



data CreateTagRequest = CreateTagRequest {
    tag_name :: Text
} deriving (Show, Generic)

instance FromJSON CreateTagRequest


data EditTagRequest = EditTagRequest {
    tag_id :: Int16,
    tag_name :: Text
} deriving (Show, Generic)

instance FromJSON EditTagRequest


data TagIdRequest = TagIdRequest {
    tag_id :: Int16
} deriving (Show, Generic)

instance FromJSON TagIdRequest

