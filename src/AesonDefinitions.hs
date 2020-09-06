{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE DeriveGeneric      #-}

module AesonDefinitions (
    CreateUserRequest(..),
    UserIdRequest(..),
    CreateCategoryRequest(..),
    UpdateCategoryRequest(..),
    CategoryIdRequest(..),
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


newtype UserIdRequest = UserIdRequest {
    user_id :: Int16
} deriving (Show, Generic)

instance FromJSON UserIdRequest



data CreateCategoryRequest = CreateCategoryRequest {
    name :: Text,
    parent_id :: Maybe Int16
} deriving (Show, Generic)

instance FromJSON CreateCategoryRequest


data UpdateCategoryRequest = UpdateCategoryRequest {
    category_id :: Int16,
    name :: Text,
    parent_id :: Maybe Int16
} deriving (Show, Generic)

instance FromJSON UpdateCategoryRequest


data CategoryIdRequest = CategoryIdRequest {
    category_id :: Int16
} deriving (Show, Generic)

instance FromJSON CategoryIdRequest



newtype CreateTagRequest = CreateTagRequest {
    tag_name :: Text
} deriving (Show, Generic)

instance FromJSON CreateTagRequest


data EditTagRequest = EditTagRequest {
    tag_id :: Int16,
    tag_name :: Text
} deriving (Show, Generic)

instance FromJSON EditTagRequest


newtype TagIdRequest = TagIdRequest {
    tag_id :: Int16
} deriving (Show, Generic)

instance FromJSON TagIdRequest

