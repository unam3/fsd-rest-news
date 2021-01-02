{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE DeriveGeneric      #-}

module AesonDefinitions (
    CreateUserRequest(..),
    UserIdRequest(..),
    AuthRequest(..),
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
    ArticleDraftEditRequest(..),
    ArticleDraftIdRequest(..),
    ArticlesByCategoryIdRequest(..),
    ArticlesByTagIdListRequest(..),
    ArticlesByTitlePartRequest(..),
    ArticlesByContentPartRequest(..),
    ArticlesByAuthorNamePartRequest(..),
    ArticlesByCreationDateRequest(..)
    ) where

import Data.Aeson (FromJSON)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Data.Vector (Vector)
import GHC.Generics (Generic)

data CreateUserRequest = CreateUserRequest {
    username :: Text,
    password :: Text,
    name :: Text,
    surname :: Text,
    avatar :: Text,
    is_admin :: Bool
} deriving (Show, Generic)

instance FromJSON CreateUserRequest


newtype UserIdRequest = UserIdRequest {
    user_id :: Int32
} deriving (Show, Generic)

instance FromJSON UserIdRequest



data AuthRequest = AuthRequest {
    username :: Text,
    password :: Text
} deriving (Show, Generic)

instance FromJSON AuthRequest



data PromoteUserToAuthorRequest = PromoteUserToAuthorRequest {
    user_id :: Int32,
    description :: Text
} deriving (Show, Generic)

instance FromJSON PromoteUserToAuthorRequest


data EditAuthorRequest = EditAuthorRequest {
    author_id :: Int32,
    user_id :: Int32,
    description :: Text
} deriving (Show, Generic)

instance FromJSON EditAuthorRequest


data AuthorIdRequest = AuthorIdRequest {
    author_id :: Int32
} deriving (Show, Generic)

instance FromJSON AuthorIdRequest



data CreateCategoryRequest = CreateCategoryRequest {
    name :: Text,
    parent_id :: Maybe Int32
} deriving (Show, Generic)

instance FromJSON CreateCategoryRequest


data UpdateCategoryRequest = UpdateCategoryRequest {
    category_id :: Int32,
    name :: Text,
    parent_id :: Maybe Int32
} deriving (Show, Generic)

instance FromJSON UpdateCategoryRequest


data CategoryIdRequest = CategoryIdRequest {
    category_id :: Int32
} deriving (Show, Generic)

instance FromJSON CategoryIdRequest



newtype CreateTagRequest = CreateTagRequest {
    tag_name :: Text
} deriving (Show, Generic)

instance FromJSON CreateTagRequest


data EditTagRequest = EditTagRequest {
    tag_id :: Int32,
    tag_name :: Text
} deriving (Show, Generic)

instance FromJSON EditTagRequest


newtype TagIdRequest = TagIdRequest {
    tag_id :: Int32
} deriving (Show, Generic)

instance FromJSON TagIdRequest



data CreateCommentRequest = CreateCommentRequest {
    article_id :: Int32,
    comment_text :: Text
} deriving (Show, Generic)

instance FromJSON CreateCommentRequest


newtype CommentIdRequest = CommentIdRequest {
    comment_id :: Int32
} deriving (Show, Generic)

instance FromJSON CommentIdRequest


data ArticleCommentsRequest = ArticleCommentsRequest {
    article_id :: Int32,
    offset :: Maybe Int32
} deriving (Show, Generic)

instance FromJSON ArticleCommentsRequest



data ArticleDraftRequest = ArticleDraftRequest {
    category_id :: Int32,
    tags :: Vector Int32,
    article_title :: Text,
    article_content :: Text,
    main_photo :: Text,
    additional_photos :: Vector Text
} deriving (Show, Generic)

instance FromJSON ArticleDraftRequest


data ArticleDraftEditRequest = ArticleDraftEditRequest {
    article_id :: Int32,
    category_id :: Int32,
    article_title :: Text,
    article_content :: Text,
    main_photo :: Text,
    additional_photos :: Vector Text
} deriving (Show, Generic)

instance FromJSON ArticleDraftEditRequest


data ArticleDraftIdRequest = ArticleDraftIdRequest {
    article_id :: Int32
} deriving (Show, Generic)

instance FromJSON ArticleDraftIdRequest




data ArticlesByCategoryIdRequest = ArticlesByCategoryIdRequest {
    category_id :: Int32,
    offset :: Maybe Int32
} deriving (Show, Generic)

instance FromJSON ArticlesByCategoryIdRequest

data ArticlesByTagIdListRequest = ArticlesByTagIdListRequest {
    tags_ids :: Vector Int32
} deriving (Show, Generic)

instance FromJSON ArticlesByTagIdListRequest

data ArticlesByTitlePartRequest = ArticlesByTitlePartRequest {
    title_substring :: Text
} deriving (Show, Generic)

instance FromJSON ArticlesByTitlePartRequest

data ArticlesByContentPartRequest = ArticlesByContentPartRequest {
    content_substring :: Text
} deriving (Show, Generic)

instance FromJSON ArticlesByContentPartRequest

data ArticlesByAuthorNamePartRequest = ArticlesByAuthorNamePartRequest {
    author_name_substring :: Text
} deriving (Show, Generic)

instance FromJSON ArticlesByAuthorNamePartRequest

data ArticlesByCreationDateRequest = ArticlesByCreationDateRequest {
    day :: Day
} deriving (Show, Generic)

instance FromJSON ArticlesByCreationDateRequest
