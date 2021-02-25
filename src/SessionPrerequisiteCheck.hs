{-# LANGUAGE OverloadedStrings  #-}

module SessionPrerequisiteCheck
    (
    RequestAndParams(..),
    endpointToEitherSessionName,
    noSuchEndpoint,
    wrongParamsOrValues
    ) where

import AesonDefinitions (AuthRequest, CreateUserRequest, UserIdRequest, PromoteUserToAuthorRequest, EditAuthorRequest, AuthorIdRequest, CreateCategoryRequest, UpdateCategoryRequest, CategoryIdRequest, CreateTagRequest, EditTagRequest, TagIdRequest, TagIdRequestWithOffset, CreateCommentRequest, CreateCommentRequest, CommentIdRequest, ArticleCommentsRequest, ArticleDraftRequest, ArticleDraftEditRequest, ArticleDraftIdRequest, ArticlesByCategoryIdRequest, ArticlesByTagIdListRequest, ArticlesByTitlePartRequest, ArticlesByContentPartRequest, ArticlesByAuthorNamePartRequest, ArticlesByCreationDateRequest, OffsetRequest)

import Data.Aeson (decode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HMS
import Data.Maybe (isJust)
import Data.Text (Text)

passSessionNameIf :: String -> Bool -> Either String String
passSessionNameIf sessionName condition = if condition
    then Right sessionName
    else noSuchEndpoint;

wrongParamsOrValues :: Either String String
wrongParamsOrValues = Left "Wrong parameters/parameters values"

passSessionNameIfValidRequest :: String -> Maybe request -> Either String String
passSessionNameIfValidRequest sessionName request = if sessionName == noSuchEndpointS
    then noSuchEndpoint
    else maybe wrongParamsOrValues (const $ Right sessionName) request

noSuchEndpointS :: String
noSuchEndpointS = "No such endpoint"

noSuchEndpoint :: Either String String
noSuchEndpoint = Left noSuchEndpointS


data RequestAndParams = Params {
    lbsRequest :: LBS.ByteString,
    isAdmin :: Bool,
    hasUserId :: Bool,
    hasAuthorId :: Bool
} deriving Show

class CheckMethods a where
    passSessionNameIfHasUserId :: a -> String -> Either String String
    passSessionNameIfHasAuthorId :: a -> String -> Either String String
    passSessionNameIfAdmin :: a -> String -> String

instance CheckMethods RequestAndParams where
    passSessionNameIfHasUserId reqAndParams sessionName = passSessionNameIf sessionName $ hasUserId reqAndParams
    passSessionNameIfHasAuthorId reqAndParams sessionName = passSessionNameIf sessionName $ hasAuthorId reqAndParams
    passSessionNameIfAdmin reqAndParams sessionName = if isAdmin reqAndParams
        then sessionName
        else noSuchEndpointS;


endpointToEitherSessionName :: HMS.HashMap ([Text], BS.ByteString) (RequestAndParams -> Either String String);
endpointToEitherSessionName = HMS.fromList [
    ((["auth"], "POST"),
        passSessionNameIfValidRequest
            "auth"
            . (decode :: LBS.ByteString -> Maybe AuthRequest) . lbsRequest
            ),
    ((["users"], "POST"),
        passSessionNameIfValidRequest
            "createUser"
            . (decode :: LBS.ByteString -> Maybe CreateUserRequest) . lbsRequest
            ),
    ((["users"], "GET"),
        (`passSessionNameIfHasUserId` "getUser")
        ),
    ((["users"], "DELETE"),
        \ params -> passSessionNameIfValidRequest
            (passSessionNameIfAdmin params "deleteUser")
            . (decode :: LBS.ByteString -> Maybe UserIdRequest) $ lbsRequest params
            ),
    ((["authors"], "POST"),
        \ params -> passSessionNameIfValidRequest
            (passSessionNameIfAdmin params "promoteUserToAuthor")
            . (decode :: LBS.ByteString -> Maybe PromoteUserToAuthorRequest) $ lbsRequest params
            ),
    ((["authors"], "PATCH"),
        \ params -> passSessionNameIfValidRequest
            (passSessionNameIfAdmin params "editAuthor")
            . (decode :: LBS.ByteString -> Maybe EditAuthorRequest) $ lbsRequest params
            ),
    ((["authors"], "GET"),
        \ params -> passSessionNameIfValidRequest
            (passSessionNameIfAdmin params "getAuthor")
            . (decode :: LBS.ByteString -> Maybe AuthorIdRequest) $ lbsRequest params
            ),
    ((["authors"], "DELETE"),
        \ params -> passSessionNameIfValidRequest
            (passSessionNameIfAdmin params "deleteAuthorRole")
            . (decode :: LBS.ByteString -> Maybe AuthorIdRequest) $ lbsRequest params
            ),
    ((["categories"], "POST"),
        \ params -> passSessionNameIfValidRequest
            (passSessionNameIfAdmin params "createCategory")
            . (decode :: LBS.ByteString -> Maybe CreateCategoryRequest) $ lbsRequest params
            ),
    ((["categories"], "PATCH"),
        \ params -> passSessionNameIfValidRequest
            (passSessionNameIfAdmin params "updateCategory")
            . (decode :: LBS.ByteString -> Maybe UpdateCategoryRequest) $ lbsRequest params
            ),
    ((["categories"], "GET"),
        passSessionNameIfValidRequest
            "getCategory"
            . (decode :: LBS.ByteString -> Maybe CategoryIdRequest) . lbsRequest
            ),
    ((["categories"], "DELETE"),
        \ params -> passSessionNameIfValidRequest
            (passSessionNameIfAdmin params "deleteCategory")
            . (decode :: LBS.ByteString -> Maybe CategoryIdRequest) $ lbsRequest params
            ),
    ((["tags"], "POST"),
        \ params -> passSessionNameIfValidRequest
            (passSessionNameIfAdmin params "createTag")
            . (decode :: LBS.ByteString -> Maybe CreateTagRequest) $ lbsRequest params
            ),
    ((["tags"], "PATCH"),
        \ params -> passSessionNameIfValidRequest
            (passSessionNameIfAdmin params "editTag")
            . (decode :: LBS.ByteString -> Maybe EditTagRequest) $ lbsRequest params
            ),
    ((["tags"], "GET"),
        passSessionNameIfValidRequest
            "getTag"
            . (decode :: LBS.ByteString -> Maybe TagIdRequest) . lbsRequest
            ),
    ((["tags"], "DELETE"),
        \ params -> passSessionNameIfValidRequest
            (passSessionNameIfAdmin params "deleteTag")
            . (decode :: LBS.ByteString -> Maybe TagIdRequest) $ lbsRequest params
            ),
    ((["comments"], "POST"),
        \ params -> passSessionNameIfValidRequest
            (either id id $ passSessionNameIfHasUserId params "createComment")
            . (decode :: LBS.ByteString -> Maybe CreateCommentRequest) $ lbsRequest params
            ),
    ((["comments"], "GET"),
        passSessionNameIfValidRequest
            "getArticleComments"
            . (decode :: LBS.ByteString -> Maybe ArticleCommentsRequest) . lbsRequest
            ),
    ((["comments"], "DELETE"),
        \ params -> passSessionNameIfValidRequest
            (either id id $ passSessionNameIfHasUserId params "deleteComment")
            . (decode :: LBS.ByteString -> Maybe CommentIdRequest) $ lbsRequest params
            ),
    ((["articles"], "POST"),
        \ params -> if isJust (decode $ lbsRequest params :: Maybe ArticleDraftRequest)
            then passSessionNameIfHasAuthorId params "createArticleDraft"
            else passSessionNameIfValidRequest
                (either id id $ passSessionNameIfHasAuthorId params "publishArticleDraft")
                (decode $ lbsRequest params :: Maybe ArticleDraftIdRequest)
                ),
    ((["articles"], "PATCH"),
        \ params -> passSessionNameIfValidRequest
            (either id id $ passSessionNameIfHasAuthorId params "editArticleDraft")
            . (decode :: LBS.ByteString -> Maybe ArticleDraftEditRequest) $ lbsRequest params
            ),
    ((["articles"], "GET"),
        \ params -> passSessionNameIfValidRequest
            (either id id $ passSessionNameIfHasAuthorId params "getArticleDraft")
            . (decode :: LBS.ByteString -> Maybe ArticleDraftIdRequest) $ lbsRequest params
            ),
    ((["articles"], "DELETE"),
        \ params -> passSessionNameIfValidRequest
            (either id id $ passSessionNameIfHasAuthorId params "deleteArticleDraft")
            . (decode :: LBS.ByteString -> Maybe ArticleDraftIdRequest) $ lbsRequest params
            ),
    ((["articles", "category"], "GET"),
        passSessionNameIfValidRequest
            "getArticlesByCategoryId"
            . (decode :: LBS.ByteString -> Maybe ArticlesByCategoryIdRequest) . lbsRequest
            ),
    ((["articles", "tag"], "GET"),
        passSessionNameIfValidRequest
            "getArticlesByTagId"
            . (decode :: LBS.ByteString -> Maybe TagIdRequestWithOffset) . lbsRequest
            ),
    ((["articles", "tags__any"], "GET"),
        passSessionNameIfValidRequest
            "getArticlesByAnyTagId"
            . (decode :: LBS.ByteString -> Maybe ArticlesByTagIdListRequest) . lbsRequest
            ),
    ((["articles", "tags__all"], "GET"),
        passSessionNameIfValidRequest
            "getArticlesByAllTagId"
            . (decode :: LBS.ByteString -> Maybe ArticlesByTagIdListRequest) . lbsRequest
            ),
    ((["articles", "in__title"], "GET"),
        passSessionNameIfValidRequest
            "getArticlesByTitlePart"
            . (decode :: LBS.ByteString -> Maybe ArticlesByTitlePartRequest) . lbsRequest
            ),
    ((["articles", "in__content"], "GET"),
        passSessionNameIfValidRequest
            "getArticlesByContentPart"
            . (decode :: LBS.ByteString -> Maybe ArticlesByContentPartRequest) . lbsRequest
            ),
    ((["articles", "in__author_name"], "GET"),
        passSessionNameIfValidRequest
            "getArticlesByAuthorNamePart"
            . (decode :: LBS.ByteString -> Maybe ArticlesByAuthorNamePartRequest) . lbsRequest
            ),
    ((["articles", "byPhotosNumber"], "GET"),
        passSessionNameIfValidRequest
            "getArticlesSortedByPhotosNumber"
            . (decode :: LBS.ByteString -> Maybe OffsetRequest) . lbsRequest
            ),
    ((["articles", "byCreationDate"], "GET"),
        passSessionNameIfValidRequest
            "getArticlesSortedByCreationDate"
            . (decode :: LBS.ByteString -> Maybe OffsetRequest) . lbsRequest
            ),
    ((["articles", "sortByAuthor"], "GET"),
        passSessionNameIfValidRequest
            "getArticlesSortedByAuthor"
            . (decode :: LBS.ByteString -> Maybe OffsetRequest) . lbsRequest
            ),
    ((["articles", "sortByCategory"], "GET"),
        passSessionNameIfValidRequest
            "getArticlesSortedByCategory"
            . (decode :: LBS.ByteString -> Maybe OffsetRequest) . lbsRequest
            ),
    ((["articles", "createdAt"], "GET"),
        passSessionNameIfValidRequest
            "getArticlesFilteredByCreationDate"
            . (decode :: LBS.ByteString -> Maybe  ArticlesByCreationDateRequest) . lbsRequest
            ),
    ((["articles", "createdBefore"], "GET"),
        passSessionNameIfValidRequest
            "getArticlesCreatedBeforeDate"
            . (decode :: LBS.ByteString -> Maybe  ArticlesByCreationDateRequest) . lbsRequest
            ),
    ((["articles", "createdAfter"], "GET"),
        passSessionNameIfValidRequest
            "getArticlesCreatedAfterDate"
            . (decode :: LBS.ByteString -> Maybe ArticlesByCreationDateRequest) . lbsRequest
        )
    ];
