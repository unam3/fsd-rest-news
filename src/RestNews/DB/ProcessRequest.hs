{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module RestNews.DB.ProcessRequest (
    HasqlSessionResults(..),
    SessionError(..),
    createUser,
    deleteUser,
    getUser,
    promoteUserToAuthor,
    editAuthor,
    getAuthor,
    deleteAuthorRole,
    createCategory,
    updateCategory,
    getCategory,
    deleteCategory,
    createTag,
    editTag,
    deleteTag,
    getTag,
    createComment,
    deleteComment,
    getArticleComments,
    createArticleDraft,
    publishArticleDraft,
    editArticleDraft,
    getArticleDraft,
    deleteArticleDraft,
    getArticlesByCategoryId,
    getArticlesByTagId,
    getArticlesByAnyTagId,
    getArticlesByAllTagId,
    getArticlesByTitlePart,
    getArticlesByContentPart,
    getArticlesByAuthorNamePart,
    getArticlesSortedByPhotosNumber,
    getArticlesSortedByCreationDate,
    getArticlesSortedByAuthor,
    getArticlesSortedByCategory,
    getArticlesFilteredByCreationDate,
    getArticlesCreatedBeforeDate,
    getArticlesCreatedAfterDate,
    getCredentials
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (Value, encode)
import Data.ByteString.Lazy.UTF8 (ByteString)
import Data.Int (Int32)
import Data.List (isPrefixOf)
import Data.Text (Text, pack)
import Data.Time.Calendar (showGregorian)
import Hasql.Connection (Connection)
import qualified Hasql.Session as Session
import Hasql.Statement (Statement)

import RestNews.DB.Errors
import qualified RestNews.DB.Request as DBR
import RestNews.Requests.JSON

-- https://hackage.haskell.org/package/hasql-1.4.4
-- https://github.com/nikita-volkov/hasql-tutorial1


type UnhandledError = String
type ErrorForUser = ByteString
newtype HasqlSessionResults successResults = H (Either (Either UnhandledError ErrorForUser) successResults)
    deriving Show


createUser :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> CreateUserRequest
    -> m (HasqlSessionResults ByteString)
createUser sessionRun connection createUserRequest =
    do
        let params = (
                username (createUserRequest :: CreateUserRequest),
                password (createUserRequest :: CreateUserRequest),
                name (createUserRequest :: CreateUserRequest),
                surname createUserRequest,
                avatar createUserRequest
                )
        sessionResults <- sessionRun (Session.statement params DBR.createUser) connection
        pure (
            case sessionResults of
                Right results -> H . Right $ encode results
                Left sessionError -> case getErrorCode sessionError of
                    Just PSQL_STRING_DATA_RIGHT_TRUNCATION -> H . Left . Right $ encode eNameAndSurnameMaxBoundOverflow
                    Just PSQL_UNIQUE_VIOLATION -> H . Left . Right $ encode eUserWithSuchUsernameAlreadyExist
                    _ -> H . Left . Left $ show sessionError
            )

deleteUser :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> UserIdRequest
    -> m (HasqlSessionResults ByteString)
deleteUser sessionRun connection deleteUserRequest =
    do
        let params = user_id (deleteUserRequest :: UserIdRequest)
        sessionResults <- sessionRun (Session.statement params DBR.deleteUser) connection
        pure (
            case sessionResults of
                Right results -> H . Right $ encode results
                Left sessionError -> case getErrorCode sessionError of
                    Just UnexpectedAmountOfRowsOrUnexpectedNull -> H . Left . Right $ encode eSuchUserDoesNotExist
                    _ -> H . Left . Left $ show sessionError
                )

getUser :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> Int32
    -> m (HasqlSessionResults ByteString)
getUser sessionRun connection userId = do
    sessionResults <- sessionRun (Session.statement userId DBR.getUser) connection
    pure (
        case sessionResults of
            Right results -> H . Right $ encode results
            Left sessionError -> case getErrorCode sessionError of
                Just UnexpectedAmountOfRowsOrUnexpectedNull -> H . Left . Right $ encode eSuchUserDoesNotExist
                _ -> H . Left . Left $ show sessionError
            )


promoteUserToAuthor :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection -> PromoteUserToAuthorRequest
    -> m (HasqlSessionResults ByteString)
promoteUserToAuthor sessionRun connection promoteUserToAuthorRequest =
    do
        let params = (
                user_id (promoteUserToAuthorRequest :: PromoteUserToAuthorRequest),
                description (promoteUserToAuthorRequest :: PromoteUserToAuthorRequest)
                )
        sessionResults <- sessionRun (Session.statement params DBR.promoteUserToAuthor) connection
        pure (
            case sessionResults of
                Right results -> H . Right $ encode results
                Left sessionError -> case getErrorCode sessionError of
                    Just PSQL_FOREIGN_KEY_VIOLATION -> H . Left . Right $ encode eSuchUserDoesNotExist
                    Just PSQL_UNIQUE_VIOLATION -> H . Left . Right $ encode eSuchUserAlreadyAuthor
                    _ -> H . Left . Left $ show sessionError
            )

editAuthor :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> EditAuthorRequest
    -> m (HasqlSessionResults ByteString)
editAuthor sessionRun connection editAuthorRequest =
    do
        let params = (
                author_id (editAuthorRequest :: EditAuthorRequest),
                description (editAuthorRequest :: EditAuthorRequest)
                )
        sessionResults <- sessionRun (Session.statement params DBR.editAuthor) connection
        pure (
            case sessionResults of
                Right results -> H . Right $ encode results
                Left sessionError -> case getErrorCode sessionError of
                    Just PSQL_STRING_DATA_RIGHT_TRUNCATION -> H . Left . Right $ encode eNameAndSurnameMaxBoundOverflow
                    Just UnexpectedAmountOfRowsOrUnexpectedNull -> H . Left . Right $ encode eSuchAuthorDoesNotExist
                    _ -> H . Left . Left $ show sessionError
            )

getAuthor :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> AuthorIdRequest
    -> m (HasqlSessionResults ByteString)
getAuthor sessionRun connection authorIdRequest =
    do
        let params = author_id (authorIdRequest :: AuthorIdRequest)
        sessionResults <- sessionRun (Session.statement params DBR.getAuthor) connection
        pure (
            case sessionResults of
                Right results -> H . Right $ encode results
                Left sessionError -> case getErrorCode sessionError of
                    Just UnexpectedAmountOfRowsOrUnexpectedNull -> H . Left . Right $ encode eSuchAuthorDoesNotExist
                    _ -> H . Left . Left $ show sessionError
            )

deleteAuthorRole :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> AuthorIdRequest
    -> m (HasqlSessionResults ByteString)
deleteAuthorRole sessionRun connection authorIdRequest =
    do
        let params = author_id (authorIdRequest :: AuthorIdRequest)
        sessionResults <- sessionRun (Session.statement params DBR.deleteAuthorRole) connection
        pure (
            case sessionResults of
                Right results -> H . Right $ encode results
                Left sessionError -> case getErrorCode sessionError of
                    Just UnexpectedAmountOfRowsOrUnexpectedNull -> H . Left . Right $ encode eSuchAuthorDoesNotExist
                    _ -> H . Left . Left $ show sessionError
            )

createCategory :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> CreateCategoryRequest
    -> m (HasqlSessionResults ByteString)
createCategory sessionRun connection createCategoryRequest =
    do
        let params = (
                name (createCategoryRequest :: CreateCategoryRequest),
                parent_id (createCategoryRequest :: CreateCategoryRequest)
                )
        sessionResults <- sessionRun (Session.statement params DBR.createCategory) connection
        pure (
            case sessionResults of
                Right results -> H . Right $ encode results
                Left sessionError -> case getErrorCode sessionError of
                    Just PSQL_STRING_DATA_RIGHT_TRUNCATION -> H . Left . Right $ encode eCategoryNameMaxBoundOverflow
                    Just PSQL_FOREIGN_KEY_VIOLATION -> H . Left . Right $ encode eParentCategoryDoesNotExist
                    _ -> H . Left . Left $ show sessionError
            )

updateCategory :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> UpdateCategoryRequest
    -> m (HasqlSessionResults ByteString)
updateCategory sessionRun connection updateCategoryRequest =
    do
        let params = (
                category_id (updateCategoryRequest :: UpdateCategoryRequest),
                name (updateCategoryRequest :: UpdateCategoryRequest),
                parent_id (updateCategoryRequest :: UpdateCategoryRequest)
                )
        sessionResults <- sessionRun (Session.statement params DBR.updateCategory) connection
        pure (
            case sessionResults of
                Right results -> H . Right $ encode results
                Left sessionError -> case getErrorCode sessionError of
                    Just PSQL_STRING_DATA_RIGHT_TRUNCATION -> H . Left . Right $ encode eCategoryNameMaxBoundOverflow
                    Just PSQL_FOREIGN_KEY_VIOLATION -> H . Left . Right $ encode eParentCategoryDoesNotExist
                    Just UnexpectedAmountOfRowsOrUnexpectedNull -> H . Left . Right $ encode eNoSuchCategory
                    _ -> H . Left . Left $ show sessionError
            )

getCategory :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> CategoryIdRequest
    -> m (HasqlSessionResults ByteString)
getCategory sessionRun connection categoryIdRequest =
    do
        let params = category_id (categoryIdRequest :: CategoryIdRequest)
        sessionResults <- sessionRun (Session.statement params DBR.getCategory) connection
        pure (
            case sessionResults of
                Right results -> H . Right $ encode results
                Left sessionError -> case getErrorCode sessionError of
                    Just UnexpectedAmountOfRowsOrUnexpectedNull -> H . Left . Right $ encode eNoSuchCategory
                    _ -> H . Left . Left $ show sessionError
            )

deleteCategory :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> CategoryIdRequest
    -> m (HasqlSessionResults ByteString)
deleteCategory sessionRun connection categoryIdRequest =
    do
        let params = category_id (categoryIdRequest :: CategoryIdRequest)
        sessionResults <- sessionRun (Session.statement params DBR.deleteCategory) connection
        pure (
            case sessionResults of
                Right results -> H . Right $ encode results
                Left sessionError -> case getErrorCode sessionError of
                    Just PSQL_FOREIGN_KEY_VIOLATION -> H . Left . Right $ encode eCategoryInUse
                    Just UnexpectedAmountOfRowsOrUnexpectedNull -> H . Left . Right $ encode eNoSuchCategory
                    _ -> H . Left . Left $ show sessionError
            )


createTag :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> CreateTagRequest
    -> m (HasqlSessionResults ByteString)
createTag sessionRun connection createTagRequest =
    do
        let params = tag_name (createTagRequest :: CreateTagRequest)
        sessionResults <- sessionRun (Session.statement params DBR.createTag) connection
        pure (
            case sessionResults of
                Right results -> H . Right $ encode results
                Left sessionError -> case getErrorCode sessionError of
                    Just PSQL_STRING_DATA_RIGHT_TRUNCATION -> H . Left . Right $ encode eTagNameMaxBoundOverflow
                    Just PSQL_UNIQUE_VIOLATION -> H . Left . Right $ encode eTagWithSuchNameAlreadyExist
                    _ -> H . Left . Left $ show sessionError
            )

editTag :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> EditTagRequest
    -> m (HasqlSessionResults ByteString)
editTag sessionRun connection editTagRequest =
    do
        let params = (tag_id (editTagRequest :: EditTagRequest), tag_name (editTagRequest :: EditTagRequest))
        sessionResults <- sessionRun (Session.statement params DBR.editTag) connection
        pure (
            case sessionResults of
                Right results -> H . Right $ encode results
                Left sessionError -> case getErrorCode sessionError of
                    Just PSQL_STRING_DATA_RIGHT_TRUNCATION -> H . Left . Right $ encode eTagNameMaxBoundOverflow
                    Just PSQL_UNIQUE_VIOLATION -> H . Left . Right $ encode eTagWithSuchNameAlreadyExist
                    Just UnexpectedAmountOfRowsOrUnexpectedNull -> H . Left . Right $ encode eNoSuchTag
                    _ -> H . Left . Left $ show sessionError
            )

deleteTag :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> TagIdRequest
    -> m (HasqlSessionResults ByteString)
deleteTag sessionRun connection deleteTagRequest =
    do
        let params = tag_id (deleteTagRequest :: TagIdRequest)
        sessionResults <- sessionRun (Session.statement params DBR.deleteTag) connection
        pure (
            case sessionResults of
                Right results -> H . Right $ encode results
                Left sessionError -> case getErrorCode sessionError of
                    Just PSQL_FOREIGN_KEY_VIOLATION -> H . Left . Right $ encode eTagReferencedByArticle
                    Just UnexpectedAmountOfRowsOrUnexpectedNull -> H . Left . Right $ encode eNoSuchTag
                    _ -> H . Left . Left $ show sessionError
            )

getTag :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> TagIdRequest
    -> m (HasqlSessionResults ByteString)
getTag sessionRun connection getTagRequest =
    do
        let params = tag_id (getTagRequest :: TagIdRequest)
        sessionResults <- sessionRun (Session.statement params DBR.getTag) connection
        pure (
            case sessionResults of
                Right results -> H . Right $ encode results
                Left sessionError -> case getErrorCode sessionError of
                    Just UnexpectedAmountOfRowsOrUnexpectedNull -> H . Left . Right $ encode eNoSuchTag
                    _ -> H . Left . Left $ show sessionError
            )


createComment :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> CreateCommentRequest
    -> Int32
    -> m (HasqlSessionResults ByteString)
createComment sessionRun connection createCommentRequest user_id' =
    do
        let params = (
                article_id (createCommentRequest :: CreateCommentRequest),
                comment_text (createCommentRequest :: CreateCommentRequest),
                user_id'
                )
        sessionResults <- sessionRun (Session.statement params DBR.createComment) connection
        pure (
            case sessionResults of
                Right results -> H . Right $ encode results
                Left sessionError -> case getErrorCode sessionError of
                    Just PSQL_FOREIGN_KEY_VIOLATION -> H . Left . Right $ encode eNoSuchArticle
                    _ -> H . Left . Left $ show sessionError
            )

deleteComment :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> CommentIdRequest
    -> Int32
    -> m (HasqlSessionResults ByteString)
deleteComment sessionRun connection deleteCommentRequest user_id' =
    do
        let params = (
                comment_id (deleteCommentRequest :: CommentIdRequest),
                user_id'
                )
        sessionResults <- sessionRun (Session.statement params DBR.deleteComment) connection
        pure (
            case sessionResults of
                Right results -> H . Right $ encode results
                Left sessionError -> case getErrorCode sessionError of
                    Just UnexpectedAmountOfRowsOrUnexpectedNull -> H . Left . Right $ encode eNoSuchComment
                    _ -> H . Left . Left $ show sessionError
            )

getArticleComments :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticleCommentsRequest
    -> m (HasqlSessionResults ByteString)
getArticleComments sessionRun connection articleCommentsRequest =
    do
        let params = (
                article_id (articleCommentsRequest :: ArticleCommentsRequest),
                offset (articleCommentsRequest :: ArticleCommentsRequest)
                )
        sessionResults <- sessionRun (Session.statement params DBR.getArticleComments) connection
        pure (
            case sessionResults of
                Right results -> H . Right $ encode results
                Left sessionError -> case getError sessionError of
                    Just (PSQL_INVALID_ROW_COUNT_IN_RESULT_OFFSET_CLAUSE, Just msg) -> if "OFFSET" `isPrefixOf` msg
                        then H . Left . Right $ encode eNegativeOffset
                        else H . Left . Left $ show sessionError
                    _ -> H . Left . Left $ show sessionError
            )


createArticleDraft :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticleDraftRequest
    -> Int32
    -> m (HasqlSessionResults ByteString)
createArticleDraft sessionRun connection articleDraftRequest author_id' =
    do
        let params = (
                author_id',
                category_id (articleDraftRequest :: ArticleDraftRequest),
                article_title (articleDraftRequest :: ArticleDraftRequest),
                article_content (articleDraftRequest :: ArticleDraftRequest),
                tags (articleDraftRequest :: ArticleDraftRequest),
                main_photo (articleDraftRequest :: ArticleDraftRequest),
                additional_photos (articleDraftRequest :: ArticleDraftRequest)
                )
        sessionResults <- sessionRun (Session.statement params DBR.createArticleDraft) connection
        pure (
            case sessionResults of
                Right results -> H . Right $ encode results
                Left sessionError -> case getError sessionError of
                    Just (PSQL_STRING_DATA_RIGHT_TRUNCATION, _) -> H . Left . Right $ encode eArticleTitleMaxBoundOverflow
                    Just (PSQL_FOREIGN_KEY_VIOLATION, details) ->
                        let {
                            detailsPrefix = fmap (take 12) details;
                        } in case detailsPrefix of
                            Just "Key (tag_id)" -> H . Left . Right $ encode eNoSuchTag
                            Just "Key (categor" -> H . Left . Right $ encode eNoSuchCategory
                            _ -> H . Left . Left $ show sessionError
                    Just (UnexpectedAmountOfRowsOrUnexpectedNull, Nothing) -> H . Left . Right $ encode eNoSuchArticle
                    _ -> H . Left . Left $ show sessionError
            )

publishArticleDraft :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticleDraftIdRequest
    -> Int32 ->
    m (HasqlSessionResults ByteString)
publishArticleDraft sessionRun connection articleDraftIdRequest author_id' =
    do
        let params = (
                author_id',
                article_id (articleDraftIdRequest :: ArticleDraftIdRequest)
                )
        sessionResults <- sessionRun (Session.statement params DBR.publishArticleDraft) connection
        pure (
            case sessionResults of
                Right results -> H . Right $ encode results
                Left sessionError -> case getErrorCode sessionError of
                    Just UnexpectedAmountOfRowsOrUnexpectedNull -> H . Left . Right $ encode eNoSuchArticle
                    _ -> H . Left . Left $ show sessionError
            )

editArticleDraft :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticleDraftEditRequest
    -> Int32
    -> m (HasqlSessionResults ByteString)
editArticleDraft sessionRun connection articleDraftEditRequest author_id' =
    do
        let params = (
                author_id',
                article_id (articleDraftEditRequest :: ArticleDraftEditRequest),
                category_id (articleDraftEditRequest :: ArticleDraftEditRequest),
                article_title (articleDraftEditRequest :: ArticleDraftEditRequest),
                article_content (articleDraftEditRequest :: ArticleDraftEditRequest),
                main_photo (articleDraftEditRequest :: ArticleDraftEditRequest),
                additional_photos (articleDraftEditRequest :: ArticleDraftEditRequest),
                tags (articleDraftEditRequest :: ArticleDraftEditRequest)
                )
        sessionResults <- sessionRun (Session.statement params DBR.editArticleDraft) connection
        pure (
            case sessionResults of
                Right results -> H . Right $ encode results
                Left sessionError -> case getError sessionError of
                    Just (PSQL_STRING_DATA_RIGHT_TRUNCATION, _) -> H . Left . Right $ encode eArticleTitleMaxBoundOverflow
                    Just (PSQL_FOREIGN_KEY_VIOLATION, details) ->
                        let {
                            detailsPrefix = fmap (take 12) details;
                        } in case detailsPrefix of
                            Just "Key (tag_id)" -> H . Left . Right $ encode eNoSuchTag
                            Just "Key (categor" -> H . Left . Right $ encode eNoSuchCategory
                            Just "Key (article" -> H . Left . Right $ encode eNoSuchArticle
                            _ -> H . Left . Left $ show sessionError
                    Just (UnexpectedAmountOfRowsOrUnexpectedNull, Nothing) -> H . Left . Right $ encode eNoSuchArticle
                    _ -> H . Left . Left $ show sessionError
            )

getArticleDraft :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticleDraftIdRequest
    -> Int32 ->
    m (HasqlSessionResults ByteString)
getArticleDraft sessionRun connection articleDraftIdRequest author_id' =
    do
        let params = (
                article_id (articleDraftIdRequest :: ArticleDraftIdRequest),
                author_id'
                )
        sessionResults <- sessionRun (Session.statement params DBR.getArticleDraft) connection
        pure (
            case sessionResults of
                Right results -> H . Right $ encode results
                Left sessionError -> case getErrorCode sessionError of
                    Just UnexpectedAmountOfRowsOrUnexpectedNull -> H . Left . Right $ encode eNoSuchArticle
                    _ -> H . Left . Left $ show sessionError
            )

deleteArticleDraft :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticleDraftIdRequest
    -> Int32
    -> m (HasqlSessionResults ByteString)
deleteArticleDraft sessionRun connection articleDraftIdRequest author_id' =
    do
        let params = (
                article_id (articleDraftIdRequest :: ArticleDraftIdRequest),
                author_id'
                )
        sessionResults <- sessionRun (Session.statement params DBR.deleteArticleDraft) connection
        pure (
            case sessionResults of
                Right results -> H . Right $ encode results
                Left sessionError -> case getErrorCode sessionError of
                    Just UnexpectedAmountOfRowsOrUnexpectedNull -> H . Left . Right $ encode eNoSuchArticle
                    _ -> H . Left . Left $ show sessionError
            )


getArticlesByCategoryId :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticlesByCategoryIdRequest
    -> m (HasqlSessionResults ByteString)
getArticlesByCategoryId sessionRun connection articlesByCategoryIdRequest =
    do
        let params = (
                category_id (articlesByCategoryIdRequest :: ArticlesByCategoryIdRequest),
                offset (articlesByCategoryIdRequest :: ArticlesByCategoryIdRequest)
                )
        sessionResults <- sessionRun (Session.statement params DBR.getArticlesByCategoryId) connection
        pure (
            case sessionResults of
                Right results -> H . Right $ encode results
                Left sessionError -> case getError sessionError of
                    Just (PSQL_INVALID_ROW_COUNT_IN_RESULT_OFFSET_CLAUSE, Just msg) -> if "OFFSET" `isPrefixOf` msg
                        then H . Left . Right $ encode eNegativeOffset
                        else H . Left . Left $ show sessionError
                    _ -> H . Left . Left $ show sessionError
            )

getArticlesByTagId :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> TagIdRequestWithOffset
    -> m (HasqlSessionResults ByteString)
getArticlesByTagId sessionRun connection tagIdRequestWithOffset =
    do
        let params = (
                tag_id (tagIdRequestWithOffset :: TagIdRequestWithOffset),
                offset (tagIdRequestWithOffset :: TagIdRequestWithOffset)
                )
        sessionResults <- sessionRun (Session.statement params DBR.getArticlesByTagId) connection
        pure (
            case sessionResults of
                Right results -> H . Right $ encode results
                Left sessionError -> case getError sessionError of
                    Just (PSQL_INVALID_ROW_COUNT_IN_RESULT_OFFSET_CLAUSE, Just msg) -> if "OFFSET" `isPrefixOf` msg
                        then H . Left . Right $ encode eNegativeOffset
                        else H . Left . Left $ show sessionError
                    _ -> H . Left . Left $ show sessionError
            )


getArticlesByAnyTagId :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticlesByTagIdListRequest
    -> m (HasqlSessionResults ByteString)
getArticlesByAnyTagId sessionRun connection tagIdsRequest =
    do
        let params = (
                tags_ids (tagIdsRequest :: ArticlesByTagIdListRequest),
                offset (tagIdsRequest :: ArticlesByTagIdListRequest)
                )
        sessionResults <- sessionRun (Session.statement params DBR.getArticlesByAnyTagId) connection
        pure (
            case sessionResults of
                Right results -> H . Right $ encode results
                Left sessionError -> case getError sessionError of
                    Just (PSQL_INVALID_ROW_COUNT_IN_RESULT_OFFSET_CLAUSE, Just msg) -> if "OFFSET" `isPrefixOf` msg
                        then H . Left . Right $ encode eNegativeOffset
                        else H . Left . Left $ show sessionError
                    _ -> H . Left . Left $ show sessionError
            )

getArticlesByAllTagId :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticlesByTagIdListRequest
    -> m (HasqlSessionResults ByteString)
getArticlesByAllTagId sessionRun connection tagIdsRequest =
    do
        let params = (
                tags_ids (tagIdsRequest :: ArticlesByTagIdListRequest),
                offset (tagIdsRequest :: ArticlesByTagIdListRequest)
                )
        sessionResults <- sessionRun (Session.statement params DBR.getArticlesByAllTagId) connection
        pure (
            case sessionResults of
                Right results -> H . Right $ encode results
                Left sessionError -> case getError sessionError of
                    Just (PSQL_INVALID_ROW_COUNT_IN_RESULT_OFFSET_CLAUSE, Just msg) -> if "OFFSET" `isPrefixOf` msg
                        then H . Left . Right $ encode eNegativeOffset
                        else H . Left . Left $ show sessionError
                    _ -> H . Left . Left $ show sessionError
            )

getArticlesByTitlePart :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticlesByTitlePartRequest
    -> m (HasqlSessionResults ByteString)
getArticlesByTitlePart sessionRun connection substringRequest =
    do
        let params = (
                title_substring (substringRequest :: ArticlesByTitlePartRequest),
                offset (substringRequest :: ArticlesByTitlePartRequest)
                )
        sessionResults <- sessionRun (Session.statement params DBR.getArticlesByTitlePart) connection
        pure (
            case sessionResults of
                Right results -> H . Right $ encode results
                Left sessionError -> case getError sessionError of
                    Just (PSQL_INVALID_ROW_COUNT_IN_RESULT_OFFSET_CLAUSE, Just msg) -> if "OFFSET" `isPrefixOf` msg
                        then H . Left . Right $ encode eNegativeOffset
                        else H . Left . Left $ show sessionError
                    _ -> H . Left . Left $ show sessionError
            )

getArticlesByContentPart :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticlesByContentPartRequest
    -> m (HasqlSessionResults ByteString)
getArticlesByContentPart sessionRun connection substringRequest =
    do
        let params = (
                content_substring (substringRequest :: ArticlesByContentPartRequest),
                offset (substringRequest :: ArticlesByContentPartRequest)
                )
        sessionResults <- sessionRun (Session.statement params DBR.getArticlesByContentPart) connection
        pure (
            case sessionResults of
                Right results -> H . Right $ encode results
                Left sessionError -> case getError sessionError of
                    Just (PSQL_INVALID_ROW_COUNT_IN_RESULT_OFFSET_CLAUSE, Just msg) -> if "OFFSET" `isPrefixOf` msg
                        then H . Left . Right $ encode eNegativeOffset
                        else H . Left . Left $ show sessionError
                    _ -> H . Left . Left $ show sessionError
            )

getArticlesByAuthorNamePart :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticlesByAuthorNamePartRequest
    -> m (HasqlSessionResults ByteString)
getArticlesByAuthorNamePart sessionRun connection substringRequest =
    do
        let params = (
                author_name_substring (substringRequest :: ArticlesByAuthorNamePartRequest),
                offset (substringRequest :: ArticlesByAuthorNamePartRequest)
                )
        sessionResults <- sessionRun (Session.statement params DBR.getArticlesByAuthorNamePart) connection
        pure (
            case sessionResults of
                Right results -> H . Right $ encode results
                Left sessionError -> case getError sessionError of
                    Just (PSQL_INVALID_ROW_COUNT_IN_RESULT_OFFSET_CLAUSE, Just msg) -> if "OFFSET" `isPrefixOf` msg
                        then H . Left . Right $ encode eNegativeOffset
                        else H . Left . Left $ show sessionError
                    _ -> H . Left . Left $ show sessionError
            )

getArticlesSortedByPhotosNumber :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> OffsetRequest
    -> m (HasqlSessionResults ByteString)
getArticlesSortedByPhotosNumber sessionRun connection request =
    do
        let params = offset (request :: OffsetRequest)
        sessionResults <- sessionRun (Session.statement params DBR.getArticlesSortedByPhotosNumber) connection
        pure (
            case sessionResults of
                Right results -> H . Right $ encode results
                Left sessionError -> case getError sessionError of
                    Just (PSQL_INVALID_ROW_COUNT_IN_RESULT_OFFSET_CLAUSE, Just msg) -> if "OFFSET" `isPrefixOf` msg
                        then H . Left . Right $ encode eNegativeOffset
                        else H . Left . Left $ show sessionError
                    _ -> H . Left . Left $ show sessionError
            )

getArticlesSortedByCreationDate :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> OffsetRequest
    -> m (HasqlSessionResults ByteString)
getArticlesSortedByCreationDate sessionRun connection request =
    do
        let params = offset (request :: OffsetRequest)
        sessionResults <- sessionRun (Session.statement params DBR.getArticlesSortedByCreationDate) connection
        pure (
            case sessionResults of
                Right results -> H . Right $ encode results
                Left sessionError -> case getError sessionError of
                    Just (PSQL_INVALID_ROW_COUNT_IN_RESULT_OFFSET_CLAUSE, Just msg) -> if "OFFSET" `isPrefixOf` msg
                        then H . Left . Right $ encode eNegativeOffset
                        else H . Left . Left $ show sessionError
                    _ -> H . Left . Left $ show sessionError
            )

getArticlesSortedByAuthor :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> OffsetRequest
    -> m (HasqlSessionResults ByteString)
getArticlesSortedByAuthor sessionRun connection request =
    do
        let params = offset (request :: OffsetRequest)
        sessionResults <- sessionRun (Session.statement params DBR.getArticlesSortedByAuthor) connection
        pure (
            case sessionResults of
                Right results -> H . Right $ encode results
                Left sessionError -> case getError sessionError of
                    Just (PSQL_INVALID_ROW_COUNT_IN_RESULT_OFFSET_CLAUSE, Just msg) -> if "OFFSET" `isPrefixOf` msg
                        then H . Left . Right $ encode eNegativeOffset
                        else H . Left . Left $ show sessionError
                    _ -> H . Left . Left $ show sessionError
            )

getArticlesSortedByCategory :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> OffsetRequest
    -> m (HasqlSessionResults ByteString)
getArticlesSortedByCategory sessionRun connection request =
    do
        let params = offset (request :: OffsetRequest)
        sessionResults <- sessionRun (Session.statement params DBR.getArticlesSortedByCategory) connection
        pure (
            case sessionResults of
                Right results -> H . Right $ encode results
                Left sessionError -> case getError sessionError of
                    Just (PSQL_INVALID_ROW_COUNT_IN_RESULT_OFFSET_CLAUSE, Just msg) -> if "OFFSET" `isPrefixOf` msg
                        then H . Left . Right $ encode eNegativeOffset
                        else H . Left . Left $ show sessionError
                    _ -> H . Left . Left $ show sessionError
            )

getArticlesFilteredBy :: MonadIO m =>
    Statement (Text, Maybe Int32) Value
    -> (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticlesByCreationDateRequest
    -> m (HasqlSessionResults ByteString)
getArticlesFilteredBy statement sessionRun connection articlesByCreationDateRequest =
    do
        let params = (
                pack . showGregorian $ day (articlesByCreationDateRequest :: ArticlesByCreationDateRequest),
                offset (articlesByCreationDateRequest :: ArticlesByCreationDateRequest)
                )
        sessionResults <- sessionRun (Session.statement params statement) connection
        pure (
            case sessionResults of
                Right results -> H . Right $ encode results
                Left sessionError -> case getError sessionError of
                    Just (PSQL_INVALID_ROW_COUNT_IN_RESULT_OFFSET_CLAUSE, Just msg) -> if "OFFSET" `isPrefixOf` msg
                        then H . Left . Right $ encode eNegativeOffset
                        else H . Left . Left $ show sessionError
                    _ -> H . Left . Left $ show sessionError
            )

getArticlesFilteredByCreationDate :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticlesByCreationDateRequest
    -> m (HasqlSessionResults ByteString)
getArticlesFilteredByCreationDate = getArticlesFilteredBy DBR.getArticlesFilteredByCreationDate

getArticlesCreatedBeforeDate :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticlesByCreationDateRequest
    -> m (HasqlSessionResults ByteString)
getArticlesCreatedBeforeDate = getArticlesFilteredBy DBR.getArticlesCreatedBeforeDate

getArticlesCreatedAfterDate :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticlesByCreationDateRequest ->
    m (HasqlSessionResults ByteString)
getArticlesCreatedAfterDate = getArticlesFilteredBy DBR.getArticlesCreatedAfterDate


getCredentials :: MonadIO m =>
    (Session.Session (Int32, Bool, Int32) -> Connection -> m (Either Session.QueryError (Int32, Bool, Int32)))
    -> Connection -> AuthRequest -> m (HasqlSessionResults (Int32, Bool, Int32))
getCredentials sessionRun connection authRequest =
    do
        let params = (
                username (authRequest :: AuthRequest),
                password (authRequest :: AuthRequest)
                )
        sessionResults <- sessionRun (Session.statement params DBR.getCredentials) connection
        pure (
            case sessionResults of
                Right results -> H $ Right results
                Left sessionError -> case getErrorCode sessionError of
                    Just UnexpectedAmountOfRowsOrUnexpectedNull -> H . Left . Right $ encode eWrongUsernameOrPassword
                    _ -> H . Left . Left $ show sessionError
            )
