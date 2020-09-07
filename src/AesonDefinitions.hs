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
    TagIdRequest(..),
    CreateCommentRequest(..),
    CommentIdRequest(..),
    ArticleCommentsRequest(..)
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



data CreateCommentRequest = CreateCommentRequest {
    news_id :: Int16,
    comment_text :: Text
} deriving (Show, Generic)

instance FromJSON CreateCommentRequest


newtype CommentIdRequest = CommentIdRequest {
    comment_id :: Int16
} deriving (Show, Generic)

instance FromJSON CommentIdRequest


newtype ArticleCommentsRequest = ArticleCommentsRequest {
    news_id :: Int16
} deriving (Show, Generic)

instance FromJSON ArticleCommentsRequest
