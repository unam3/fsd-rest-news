{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE DeriveGeneric      #-}

module AesonDefinitions (
    CreateUserRequest(..),
    UserIdRequest(..),
    PromoteUserToAuthorRequest(..),
    EditAuthorRequest(..),
    AuthorIdRequest(..),
    CreateCategoryRequest(..),
    UpdateCategoryRequest(..),
    CategoryIdRequest(..),
    CreateTagRequest(..),
    EditTagRequest(..),
    TagIdRequest(..),
    CreateCommentRequest(..),
    CommentIdRequest(..),
    ArticleCommentsRequest(..),
    ArticleDraftRequest(..),
    ArticleDraftIdRequest(..),
    ArticleDraft(..)
    ) where

import Data.Aeson --(FromJSON, ToJSON)
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



data PromoteUserToAuthorRequest = PromoteUserToAuthorRequest {
    user_id :: Int16,
    description :: Text
} deriving (Show, Generic)

instance FromJSON PromoteUserToAuthorRequest


data EditAuthorRequest = EditAuthorRequest {
    author_id :: Int16,
    user_id :: Int16,
    description :: Text
} deriving (Show, Generic)

instance FromJSON EditAuthorRequest


data AuthorIdRequest = AuthorIdRequest {
    author_id :: Int16
} deriving (Show, Generic)

instance FromJSON AuthorIdRequest



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
    article_id :: Int16,
    comment_text :: Text
} deriving (Show, Generic)

instance FromJSON CreateCommentRequest


newtype CommentIdRequest = CommentIdRequest {
    comment_id :: Int16
} deriving (Show, Generic)

instance FromJSON CommentIdRequest


newtype ArticleCommentsRequest = ArticleCommentsRequest {
    article_id :: Int16
} deriving (Show, Generic)

instance FromJSON ArticleCommentsRequest



data ArticleDraftRequest = ArticleDraftRequest {
    author :: Int16,
    category_id :: Int16,
    --tags :: Text,
    article_title :: Text,
    article_content :: Text
} deriving (Show, Generic)

instance FromJSON ArticleDraftRequest


data ArticleDraftIdRequest = ArticleDraftIdRequest {
    article_id :: Int16
} deriving (Show, Generic)

instance FromJSON ArticleDraftIdRequest


data ArticleDraft = ArticleDraft {
    article_id :: Int16,
    article_title :: Text,
    categories :: [Category]
} deriving (Show, Generic)

instance ToJSON ArticleDraft where
    toEncoding = genericToEncoding defaultOptions

data Category = Category {
    category_id :: Int16,
    parent_id :: Int16,
    name :: Text
} deriving (Show, Generic)

instance ToJSON Category where
    toEncoding = genericToEncoding defaultOptions
