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
    eSameParentId,
    eParentIdIsDescendant,
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
    getArticlesBySubstring,
    getArticlesSortedByPhotosNumber,
    getArticlesSortedByCreationDate,
    getArticlesSortedByAuthor,
    getArticlesSortedByCategory,
    getArticlesFilteredByCreationDate,
    getArticlesCreatedBeforeDate,
    getArticlesCreatedAfterDate,
    getCredentials
    ) where

import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value, encode)
import Data.ByteString.Lazy.UTF8 (ByteString)
import Data.Int (Int32)
import Data.List (isPrefixOf)
import Data.Text (Text, pack)
import Data.Time.Calendar (showGregorian)
import Data.Vector (Vector, elem)
import Hasql.Connection (Connection)
import qualified Hasql.Session as Session
import Hasql.Statement (Statement)

import RestNews.DB.Errors
import qualified RestNews.DB.Request as DBR
import RestNews.Requests.JSON
import RestNews.Types (Error (..))

-- https://hackage.haskell.org/package/hasql-1.4.4
-- https://github.com/nikita-volkov/hasql-tutorial1


type UnhandledError = String
type ErrorForUser = ByteString
newtype HasqlSessionResults successResults = H (Either UnhandledError (Either ErrorForUser successResults))
    deriving Show


createUser :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> CreateUserRequest
    -> m (HasqlSessionResults ByteString)
createUser sessionRun connection createUserRequest = do
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
            Right results -> H . Right . Right $ encode results
            Left sessionError -> case getErrorCode sessionError of
                Just PSQL_STRING_DATA_RIGHT_TRUNCATION -> H . Right . Left $ encode eNameAndSurnameMaxBoundOverflow
                Just PSQL_UNIQUE_VIOLATION -> H . Right . Left $ encode eUserWithSuchUsernameAlreadyExist
                _ -> H . Left $ show sessionError
        )

deleteUser :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> UserIdRequest
    -> m (HasqlSessionResults ByteString)
deleteUser sessionRun connection deleteUserRequest = do
    let params = user_id (deleteUserRequest :: UserIdRequest)
    sessionResults <- sessionRun (Session.statement params DBR.deleteUser) connection
    pure (
        case sessionResults of
            Right results -> H . Right . Right $ encode results
            Left sessionError -> case getErrorCode sessionError of
                Just UnexpectedAmountOfRowsOrUnexpectedNull -> H . Right . Left $ encode eSuchUserDoesNotExist
                _ -> H . Left $ show sessionError
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
            Right results -> H . Right . Right $ encode results
            Left sessionError -> case getErrorCode sessionError of
                Just UnexpectedAmountOfRowsOrUnexpectedNull -> H . Right . Left $ encode eSuchUserDoesNotExist
                _ -> H . Left $ show sessionError
            )


promoteUserToAuthor :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection -> PromoteUserToAuthorRequest
    -> m (HasqlSessionResults ByteString)
promoteUserToAuthor sessionRun connection promoteUserToAuthorRequest = do
    let params = (
            user_id (promoteUserToAuthorRequest :: PromoteUserToAuthorRequest),
            description (promoteUserToAuthorRequest :: PromoteUserToAuthorRequest)
            )
    sessionResults <- sessionRun (Session.statement params DBR.promoteUserToAuthor) connection
    pure (
        case sessionResults of
            Right results -> H . Right . Right $ encode results
            Left sessionError -> case getErrorCode sessionError of
                Just PSQL_FOREIGN_KEY_VIOLATION -> H . Right . Left $ encode eSuchUserDoesNotExist
                Just PSQL_UNIQUE_VIOLATION -> H . Right . Left $ encode eSuchUserAlreadyAuthor
                _ -> H . Left $ show sessionError
        )

editAuthor :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> EditAuthorRequest
    -> m (HasqlSessionResults ByteString)
editAuthor sessionRun connection editAuthorRequest = do
    let params = (
            author_id (editAuthorRequest :: EditAuthorRequest),
            description (editAuthorRequest :: EditAuthorRequest)
            )
    sessionResults <- sessionRun (Session.statement params DBR.editAuthor) connection
    pure (
        case sessionResults of
            Right results -> H . Right . Right $ encode results
            Left sessionError -> case getErrorCode sessionError of
                Just PSQL_STRING_DATA_RIGHT_TRUNCATION -> H . Right . Left $ encode eNameAndSurnameMaxBoundOverflow
                Just UnexpectedAmountOfRowsOrUnexpectedNull -> H . Right . Left $ encode eSuchAuthorDoesNotExist
                _ -> H . Left $ show sessionError
        )

getAuthor :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> AuthorIdRequest
    -> m (HasqlSessionResults ByteString)
getAuthor sessionRun connection authorIdRequest = do
    let params = author_id (authorIdRequest :: AuthorIdRequest)
    sessionResults <- sessionRun (Session.statement params DBR.getAuthor) connection
    pure (
        case sessionResults of
            Right results -> H . Right . Right $ encode results
            Left sessionError -> case getErrorCode sessionError of
                Just UnexpectedAmountOfRowsOrUnexpectedNull -> H . Right . Left $ encode eSuchAuthorDoesNotExist
                _ -> H . Left $ show sessionError
        )

deleteAuthorRole :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> AuthorIdRequest
    -> m (HasqlSessionResults ByteString)
deleteAuthorRole sessionRun connection authorIdRequest = do
    let params = author_id (authorIdRequest :: AuthorIdRequest)
    sessionResults <- sessionRun (Session.statement params DBR.deleteAuthorRole) connection
    pure (
        case sessionResults of
            Right results -> H . Right . Right $ encode results
            Left sessionError -> case getErrorCode sessionError of
                Just UnexpectedAmountOfRowsOrUnexpectedNull -> H . Right . Left $ encode eSuchAuthorDoesNotExist
                _ -> H . Left $ show sessionError
        )

createCategory :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> CreateCategoryRequest
    -> m (HasqlSessionResults ByteString)
createCategory sessionRun connection createCategoryRequest = do
    let params@(_, maybe_parent_id') = (
            name (createCategoryRequest :: CreateCategoryRequest),
            parent_id (createCategoryRequest :: CreateCategoryRequest)
            )
    sessionResults <- sessionRun (Session.statement params DBR.createCategory) connection
    pure (
        case sessionResults of
            Right results -> H . Right . Right $ encode results
            Left sessionError -> case getErrorCode sessionError of
                Just PSQL_STRING_DATA_RIGHT_TRUNCATION -> H . Right . Left $ encode eCategoryNameMaxBoundOverflow
                Just PSQL_FOREIGN_KEY_VIOLATION ->
                    case maybe_parent_id' of
                        Just parent_id' -> H . Right . Left . encode . makeNoSuchCategory . pack $ show parent_id'
                        _ ->  H . Left $ show sessionError
                _ -> H . Left $ show sessionError
        )


eSameParentId :: Error
eSameParentId = Error "\\\"parent_id\\\" must be different than \\\"category_id\\\""

eParentIdIsDescendant :: Error
eParentIdIsDescendant = Error "parent can't be descendant of category"

updateCategory' :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> (Int32, Text, Maybe Int32)
    -> m (HasqlSessionResults ByteString)
updateCategory' sessionRun connection params@(category_id', _, maybe_parent_id') = do
    sessionResults <- sessionRun (Session.statement params DBR.updateCategory) connection
    pure (
        case sessionResults of
            Right results -> H . Right . Right $ encode results
            Left sessionError -> case getErrorCode sessionError of
                Just PSQL_STRING_DATA_RIGHT_TRUNCATION -> H . Right . Left $ encode eCategoryNameMaxBoundOverflow
                Just PSQL_FOREIGN_KEY_VIOLATION ->
                    case maybe_parent_id' of
                        Just parent_id' -> H . Right . Left . encode . makeNoSuchCategory . pack $ show parent_id'
                        _ ->  H . Left $ show sessionError
                Just UnexpectedAmountOfRowsOrUnexpectedNull ->
                    H . Right . Left . encode . makeNoSuchCategory . pack $ show category_id'
                _ -> H . Left $ show sessionError
        )


getCategoryDescendants :: MonadIO m =>
    Connection
    -> Int32
    -> m (Either Session.QueryError (Vector Int32))
getCategoryDescendants connection category_id' =
    liftIO
        $ Session.run
            (Session.statement category_id' DBR.getCategoryDescendants)
            connection


sameCategoryIdCheck :: MonadIO m =>
    Int32
    -> Int32
    -> m (Either (Either UnhandledError ErrorForUser) ())
sameCategoryIdCheck category_id' parent_id' =
    if category_id' == parent_id'
        then pure . Left . Right $ encode eSameParentId
        else pure $ Right ()


isCategoryExist :: MonadIO m =>
    Connection
    -> Int32
    -> m (Either Session.QueryError Bool)
isCategoryExist connection category_id' =
    liftIO
        $ Session.run
            (Session.statement category_id' DBR.isCategoryExist)
            connection

isParentCategoryExist :: MonadIO m =>
    Connection
    -> Int32
    -> m (Either (Either UnhandledError ErrorForUser) ())
isParentCategoryExist connection parent_id' = do
    
    sessionResults <- isCategoryExist connection parent_id'

    pure $ case sessionResults of
        Right isCategoryExist' ->
            if isCategoryExist'
                then Right ()
                else Left . Right . encode . makeNoSuchCategory . pack $ show parent_id'
        Left sessionError -> Left . Left $ show sessionError


isParentIdDescendant :: MonadIO m =>
    Connection
    -> (Int32, Int32)
    -> m (Either (Either UnhandledError ErrorForUser) ())
isParentIdDescendant connection (category_id', parent_id') = do

    eitherDescendants <- getCategoryDescendants connection category_id'

    case eitherDescendants of

        Left sessionError -> pure . Left . Right $ encode parentIdDescendant

        Right descendants ->
            let isParentIdDescendant' = Data.Vector.elem parent_id' descendants
            in if isParentIdDescendant'
                then pure . Left . Right $ encode eParentIdIsDescendant
                else pure $ Right ()


updateCategory :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> UpdateCategoryRequest
    -> m (HasqlSessionResults ByteString)
updateCategory sessionRun connection updateCategoryRequest = do
    let params@(category_id', _, maybe_parent_id') = (
            category_id (updateCategoryRequest :: UpdateCategoryRequest),
            name (updateCategoryRequest :: UpdateCategoryRequest),
            parent_id (updateCategoryRequest :: UpdateCategoryRequest)
            )

    case maybe_parent_id' of
        Nothing -> updateCategory' sessionRun connection params

        Just parent_id' -> do

            let checks = ExceptT (sameCategoryIdCheck category_id' parent_id')
                    >> ExceptT (isParentCategoryExist connection parent_id')
                        >> ExceptT (isParentIdDescendant connection (category_id', parent_id'))

            checkResults <- runExceptT checks

            case checkResults of
                Right _ -> updateCategory' sessionRun connection params
                Left (Left sessionError) -> pure . H $ Left sessionError
                Left (Right errorForUser) -> pure . H . Right $ Left errorForUser


getCategory :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> CategoryIdRequest
    -> m (HasqlSessionResults ByteString)
getCategory sessionRun connection categoryIdRequest = do
    let params = category_id (categoryIdRequest :: CategoryIdRequest)
    sessionResults <- sessionRun (Session.statement params DBR.getCategory) connection
    pure (
        case sessionResults of
            Right results -> H . Right . Right $ encode results
            Left sessionError -> case getErrorCode sessionError of
                Just UnexpectedAmountOfRowsOrUnexpectedNull ->
                    H . Right . Left . encode . makeNoSuchCategory . pack $ show params
                _ -> H . Left $ show sessionError
        )

deleteCategory :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> CategoryIdRequest
    -> m (HasqlSessionResults ByteString)
deleteCategory sessionRun connection categoryIdRequest = do
    let params = category_id (categoryIdRequest :: CategoryIdRequest)
    sessionResults <- sessionRun (Session.statement params DBR.deleteCategory) connection
    pure (
        case sessionResults of
            Right results -> H . Right . Right $ encode results
            Left sessionError -> case getErrorCode sessionError of
                Just PSQL_FOREIGN_KEY_VIOLATION -> H . Right . Left $ encode eCategoryInUse
                Just UnexpectedAmountOfRowsOrUnexpectedNull ->
                    H . Right . Left . encode . makeNoSuchCategory . pack $ show params
                _ -> H . Left $ show sessionError
        )


createTag :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> CreateTagRequest
    -> m (HasqlSessionResults ByteString)
createTag sessionRun connection createTagRequest = do
    let params = tag_name (createTagRequest :: CreateTagRequest)
    sessionResults <- sessionRun (Session.statement params DBR.createTag) connection
    pure (
        case sessionResults of
            Right results -> H . Right . Right $ encode results
            Left sessionError -> case getErrorCode sessionError of
                Just PSQL_STRING_DATA_RIGHT_TRUNCATION -> H . Right . Left $ encode eTagNameMaxBoundOverflow
                Just PSQL_UNIQUE_VIOLATION -> H . Right . Left $ encode eTagWithSuchNameAlreadyExist
                _ -> H . Left $ show sessionError
        )

editTag :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> EditTagRequest
    -> m (HasqlSessionResults ByteString)
editTag sessionRun connection editTagRequest = do
    let params = (tag_id (editTagRequest :: EditTagRequest), tag_name (editTagRequest :: EditTagRequest))
    sessionResults <- sessionRun (Session.statement params DBR.editTag) connection
    pure (
        case sessionResults of
            Right results -> H . Right . Right $ encode results
            Left sessionError -> case getErrorCode sessionError of
                Just PSQL_STRING_DATA_RIGHT_TRUNCATION -> H . Right . Left $ encode eTagNameMaxBoundOverflow
                Just PSQL_UNIQUE_VIOLATION -> H . Right . Left $ encode eTagWithSuchNameAlreadyExist
                Just UnexpectedAmountOfRowsOrUnexpectedNull -> H . Right . Left $ encode eNoSuchTag
                _ -> H . Left $ show sessionError
        )

deleteTag :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> TagIdRequest
    -> m (HasqlSessionResults ByteString)
deleteTag sessionRun connection deleteTagRequest = do
    let params = tag_id (deleteTagRequest :: TagIdRequest)
    sessionResults <- sessionRun (Session.statement params DBR.deleteTag) connection
    pure (
        case sessionResults of
            Right results -> H . Right . Right $ encode results
            Left sessionError -> case getErrorCode sessionError of
                Just PSQL_FOREIGN_KEY_VIOLATION -> H . Right . Left $ encode eTagReferencedByArticle
                Just UnexpectedAmountOfRowsOrUnexpectedNull -> H . Right . Left $ encode eNoSuchTag
                _ -> H . Left $ show sessionError
        )

getTag :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> TagIdRequest
    -> m (HasqlSessionResults ByteString)
getTag sessionRun connection getTagRequest = do
    let params = tag_id (getTagRequest :: TagIdRequest)
    sessionResults <- sessionRun (Session.statement params DBR.getTag) connection
    pure (
        case sessionResults of
            Right results -> H . Right . Right $ encode results
            Left sessionError -> case getErrorCode sessionError of
                Just UnexpectedAmountOfRowsOrUnexpectedNull -> H . Right . Left $ encode eNoSuchTag
                _ -> H . Left $ show sessionError
        )


createComment :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> CreateCommentRequest
    -> Int32
    -> m (HasqlSessionResults ByteString)
createComment sessionRun connection createCommentRequest user_id' = do
    let params = (
            article_id (createCommentRequest :: CreateCommentRequest),
            comment_text (createCommentRequest :: CreateCommentRequest),
            user_id'
            )
    sessionResults <- sessionRun (Session.statement params DBR.createComment) connection
    pure (
        case sessionResults of
            Right results -> H . Right . Right $ encode results
            Left sessionError -> case getErrorCode sessionError of
                Just PSQL_FOREIGN_KEY_VIOLATION -> H . Right . Left $ encode eNoSuchArticle
                _ -> H . Left $ show sessionError
        )

deleteComment :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> CommentIdRequest
    -> Int32
    -> m (HasqlSessionResults ByteString)
deleteComment sessionRun connection deleteCommentRequest user_id' = do
    let params = (
            comment_id (deleteCommentRequest :: CommentIdRequest),
            user_id'
            )
    sessionResults <- sessionRun (Session.statement params DBR.deleteComment) connection
    pure (
        case sessionResults of
            Right results -> H . Right . Right $ encode results
            Left sessionError -> case getErrorCode sessionError of
                Just UnexpectedAmountOfRowsOrUnexpectedNull -> H . Right . Left $ encode eNoSuchComment
                _ -> H . Left $ show sessionError
        )

getArticleComments :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticleCommentsRequest
    -> m (HasqlSessionResults ByteString)
getArticleComments sessionRun connection articleCommentsRequest = do
    let params = (
            article_id (articleCommentsRequest :: ArticleCommentsRequest),
            offset (articleCommentsRequest :: ArticleCommentsRequest)
            )
    sessionResults <- sessionRun (Session.statement params DBR.getArticleComments) connection
    pure (
        case sessionResults of
            Right results -> H . Right . Right $ encode results
            Left sessionError -> case getError sessionError of
                Just (PSQL_INVALID_ROW_COUNT_IN_RESULT_OFFSET_CLAUSE, Just msg) -> if "OFFSET" `isPrefixOf` msg
                    then H . Right . Left $ encode eNegativeOffset
                    else H . Left $ show sessionError
                _ -> H . Left $ show sessionError
        )


createArticleDraft :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticleDraftRequest
    -> Int32
    -> m (HasqlSessionResults ByteString)
createArticleDraft sessionRun connection articleDraftRequest author_id' = do
    let params@(_, category_id', _, _, _, _, _) = (
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
            Right results -> H . Right . Right $ encode results
            Left sessionError -> case getError sessionError of
                Just (PSQL_STRING_DATA_RIGHT_TRUNCATION, _) -> H . Right . Left $ encode eArticleTitleMaxBoundOverflow
                Just (PSQL_FOREIGN_KEY_VIOLATION, details) ->
                    let {
                        detailsPrefix = fmap (take 12) details;
                    } in case detailsPrefix of
                        Just "Key (tag_id)" -> H . Right . Left $ encode eNoSuchTag
                        Just "Key (categor" ->
                            H . Right . Left . encode . makeNoSuchCategory . pack $ show category_id'
                        _ -> H . Left $ show sessionError
                Just (UnexpectedAmountOfRowsOrUnexpectedNull, Nothing) -> H . Right . Left $ encode eNoSuchArticle
                _ -> H . Left $ show sessionError
        )

publishArticleDraft :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticleDraftIdRequest
    -> Int32 ->
    m (HasqlSessionResults ByteString)
publishArticleDraft sessionRun connection articleDraftIdRequest author_id' = do
    let params = (
            author_id',
            article_id (articleDraftIdRequest :: ArticleDraftIdRequest)
            )
    sessionResults <- sessionRun (Session.statement params DBR.publishArticleDraft) connection
    pure (
        case sessionResults of
            Right results -> H . Right . Right $ encode results
            Left sessionError -> case getErrorCode sessionError of
                Just UnexpectedAmountOfRowsOrUnexpectedNull -> H . Right . Left $ encode eNoSuchArticle
                _ -> H . Left $ show sessionError
        )

editArticleDraft :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticleDraftEditRequest
    -> Int32
    -> m (HasqlSessionResults ByteString)
editArticleDraft sessionRun connection articleDraftEditRequest author_id' = do
    let params@(_, _, category_id', _, _, _, _, _) = (
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
            Right results -> H . Right . Right $ encode results
            Left sessionError -> case getError sessionError of
                Just (PSQL_STRING_DATA_RIGHT_TRUNCATION, _) -> H . Right . Left $ encode eArticleTitleMaxBoundOverflow
                Just (PSQL_FOREIGN_KEY_VIOLATION, details) ->
                    let {
                        detailsPrefix = fmap (take 12) details;
                    } in case detailsPrefix of
                        Just "Key (tag_id)" -> H . Right . Left $ encode eNoSuchTag
                        Just "Key (categor" ->
                            H . Right . Left . encode . makeNoSuchCategory . pack $ show category_id'
                        Just "Key (article" -> H . Right . Left $ encode eNoSuchArticle
                        _ -> H . Left $ show sessionError
                Just (UnexpectedAmountOfRowsOrUnexpectedNull, Nothing) -> H . Right . Left $ encode eNoSuchArticle
                _ -> H . Left $ show sessionError
        )

getArticleDraft :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticleDraftIdRequest
    -> Int32 ->
    m (HasqlSessionResults ByteString)
getArticleDraft sessionRun connection articleDraftIdRequest author_id' = do
    let params = (
            article_id (articleDraftIdRequest :: ArticleDraftIdRequest),
            author_id'
            )
    sessionResults <- sessionRun (Session.statement params DBR.getArticleDraft) connection
    pure (
        case sessionResults of
            Right results -> H . Right . Right $ encode results
            Left sessionError -> case getErrorCode sessionError of
                Just UnexpectedAmountOfRowsOrUnexpectedNull -> H . Right . Left $ encode eNoSuchArticle
                _ -> H . Left $ show sessionError
        )

deleteArticleDraft :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticleDraftIdRequest
    -> Int32
    -> m (HasqlSessionResults ByteString)
deleteArticleDraft sessionRun connection articleDraftIdRequest author_id' = do
    let params = (
            article_id (articleDraftIdRequest :: ArticleDraftIdRequest),
            author_id'
            )
    sessionResults <- sessionRun (Session.statement params DBR.deleteArticleDraft) connection
    pure (
        case sessionResults of
            Right results -> H . Right . Right $ encode results
            Left sessionError -> case getErrorCode sessionError of
                Just UnexpectedAmountOfRowsOrUnexpectedNull -> H . Right . Left $ encode eNoSuchArticle
                _ -> H . Left $ show sessionError
        )


getArticlesByCategoryId :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticlesByCategoryIdRequest
    -> m (HasqlSessionResults ByteString)
getArticlesByCategoryId sessionRun connection articlesByCategoryIdRequest = do
    let params = (
            category_id (articlesByCategoryIdRequest :: ArticlesByCategoryIdRequest),
            offset (articlesByCategoryIdRequest :: ArticlesByCategoryIdRequest)
            )
    sessionResults <- sessionRun (Session.statement params DBR.getArticlesByCategoryId) connection
    pure (
        case sessionResults of
            Right results -> H . Right . Right $ encode results
            Left sessionError -> case getError sessionError of
                Just (PSQL_INVALID_ROW_COUNT_IN_RESULT_OFFSET_CLAUSE, Just msg) -> if "OFFSET" `isPrefixOf` msg
                    then H . Right . Left $ encode eNegativeOffset
                    else H . Left $ show sessionError
                _ -> H . Left $ show sessionError
        )

getArticlesByTagId :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> TagIdRequestWithOffset
    -> m (HasqlSessionResults ByteString)
getArticlesByTagId sessionRun connection tagIdRequestWithOffset = do
    let params = (
            tag_id (tagIdRequestWithOffset :: TagIdRequestWithOffset),
            offset (tagIdRequestWithOffset :: TagIdRequestWithOffset)
            )
    sessionResults <- sessionRun (Session.statement params DBR.getArticlesByTagId) connection
    pure (
        case sessionResults of
            Right results -> H . Right . Right $ encode results
            Left sessionError -> case getError sessionError of
                Just (PSQL_INVALID_ROW_COUNT_IN_RESULT_OFFSET_CLAUSE, Just msg) -> if "OFFSET" `isPrefixOf` msg
                    then H . Right . Left $ encode eNegativeOffset
                    else H . Left $ show sessionError
                _ -> H . Left $ show sessionError
        )


getArticlesByAnyTagId :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticlesByTagIdListRequest
    -> m (HasqlSessionResults ByteString)
getArticlesByAnyTagId sessionRun connection tagIdsRequest = do
    let params = (
            tags_ids (tagIdsRequest :: ArticlesByTagIdListRequest),
            offset (tagIdsRequest :: ArticlesByTagIdListRequest)
            )
    sessionResults <- sessionRun (Session.statement params DBR.getArticlesByAnyTagId) connection
    pure (
        case sessionResults of
            Right results -> H . Right . Right $ encode results
            Left sessionError -> case getError sessionError of
                Just (PSQL_INVALID_ROW_COUNT_IN_RESULT_OFFSET_CLAUSE, Just msg) -> if "OFFSET" `isPrefixOf` msg
                    then H . Right . Left $ encode eNegativeOffset
                    else H . Left $ show sessionError
                _ -> H . Left $ show sessionError
        )

getArticlesByAllTagId :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticlesByTagIdListRequest
    -> m (HasqlSessionResults ByteString)
getArticlesByAllTagId sessionRun connection tagIdsRequest = do
    let params = (
            tags_ids (tagIdsRequest :: ArticlesByTagIdListRequest),
            offset (tagIdsRequest :: ArticlesByTagIdListRequest)
            )
    sessionResults <- sessionRun (Session.statement params DBR.getArticlesByAllTagId) connection
    pure (
        case sessionResults of
            Right results -> H . Right . Right $ encode results
            Left sessionError -> case getError sessionError of
                Just (PSQL_INVALID_ROW_COUNT_IN_RESULT_OFFSET_CLAUSE, Just msg) -> if "OFFSET" `isPrefixOf` msg
                    then H . Right . Left $ encode eNegativeOffset
                    else H . Left $ show sessionError
                _ -> H . Left $ show sessionError
        )

getArticlesByTitlePart :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticlesByTitlePartRequest
    -> m (HasqlSessionResults ByteString)
getArticlesByTitlePart sessionRun connection substringRequest = do
    let params = (
            title_substring (substringRequest :: ArticlesByTitlePartRequest),
            offset (substringRequest :: ArticlesByTitlePartRequest)
            )
    sessionResults <- sessionRun (Session.statement params DBR.getArticlesByTitlePart) connection
    pure (
        case sessionResults of
            Right results -> H . Right . Right $ encode results
            Left sessionError -> case getError sessionError of
                Just (PSQL_INVALID_ROW_COUNT_IN_RESULT_OFFSET_CLAUSE, Just msg) -> if "OFFSET" `isPrefixOf` msg
                    then H . Right . Left $ encode eNegativeOffset
                    else H . Left $ show sessionError
                _ -> H . Left $ show sessionError
        )

getArticlesByContentPart :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticlesByContentPartRequest
    -> m (HasqlSessionResults ByteString)
getArticlesByContentPart sessionRun connection substringRequest = do
    let params = (
            content_substring (substringRequest :: ArticlesByContentPartRequest),
            offset (substringRequest :: ArticlesByContentPartRequest)
            )
    sessionResults <- sessionRun (Session.statement params DBR.getArticlesByContentPart) connection
    pure (
        case sessionResults of
            Right results -> H . Right . Right $ encode results
            Left sessionError -> case getError sessionError of
                Just (PSQL_INVALID_ROW_COUNT_IN_RESULT_OFFSET_CLAUSE, Just msg) -> if "OFFSET" `isPrefixOf` msg
                    then H . Right . Left $ encode eNegativeOffset
                    else H . Left $ show sessionError
                _ -> H . Left $ show sessionError
        )

getArticlesByAuthorNamePart :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticlesByAuthorNamePartRequest
    -> m (HasqlSessionResults ByteString)
getArticlesByAuthorNamePart sessionRun connection substringRequest = do
    let params = (
            author_name_substring (substringRequest :: ArticlesByAuthorNamePartRequest),
            offset (substringRequest :: ArticlesByAuthorNamePartRequest)
            )
    sessionResults <- sessionRun (Session.statement params DBR.getArticlesByAuthorNamePart) connection
    pure (
        case sessionResults of
            Right results -> H . Right . Right $ encode results
            Left sessionError -> case getError sessionError of
                Just (PSQL_INVALID_ROW_COUNT_IN_RESULT_OFFSET_CLAUSE, Just msg) -> if "OFFSET" `isPrefixOf` msg
                    then H . Right . Left $ encode eNegativeOffset
                    else H . Left $ show sessionError
                _ -> H . Left $ show sessionError
        )

getArticlesBySubstring :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticlesByTextContentRequest
    -> m (HasqlSessionResults ByteString)
getArticlesBySubstring sessionRun connection substringRequest = do
    let params = (
            substring (substringRequest :: ArticlesByTextContentRequest),
            offset (substringRequest :: ArticlesByTextContentRequest)
            )
    sessionResults <- sessionRun (Session.statement params DBR.getArticlesBySubstring) connection
    pure (
        case sessionResults of
            Right results -> H . Right . Right $ encode results
            Left sessionError -> case getError sessionError of
                Just (PSQL_INVALID_ROW_COUNT_IN_RESULT_OFFSET_CLAUSE, Just msg) -> if "OFFSET" `isPrefixOf` msg
                    then H . Right . Left $ encode eNegativeOffset
                    else H . Left $ show sessionError
                _ -> H . Left $ show sessionError
        )

getArticlesSortedByPhotosNumber :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> OffsetRequest
    -> m (HasqlSessionResults ByteString)
getArticlesSortedByPhotosNumber sessionRun connection request = do
    let params = offset (request :: OffsetRequest)
    sessionResults <- sessionRun (Session.statement params DBR.getArticlesSortedByPhotosNumber) connection
    pure (
        case sessionResults of
            Right results -> H . Right . Right $ encode results
            Left sessionError -> case getError sessionError of
                Just (PSQL_INVALID_ROW_COUNT_IN_RESULT_OFFSET_CLAUSE, Just msg) -> if "OFFSET" `isPrefixOf` msg
                    then H . Right . Left $ encode eNegativeOffset
                    else H . Left $ show sessionError
                _ -> H . Left $ show sessionError
        )

getArticlesSortedByCreationDate :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> OffsetRequest
    -> m (HasqlSessionResults ByteString)
getArticlesSortedByCreationDate sessionRun connection request = do
    let params = offset (request :: OffsetRequest)
    sessionResults <- sessionRun (Session.statement params DBR.getArticlesSortedByCreationDate) connection
    pure (
        case sessionResults of
            Right results -> H . Right . Right $ encode results
            Left sessionError -> case getError sessionError of
                Just (PSQL_INVALID_ROW_COUNT_IN_RESULT_OFFSET_CLAUSE, Just msg) -> if "OFFSET" `isPrefixOf` msg
                    then H . Right . Left $ encode eNegativeOffset
                    else H . Left $ show sessionError
                _ -> H . Left $ show sessionError
        )

getArticlesSortedByAuthor :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> OffsetRequest
    -> m (HasqlSessionResults ByteString)
getArticlesSortedByAuthor sessionRun connection request = do
    let params = offset (request :: OffsetRequest)
    sessionResults <- sessionRun (Session.statement params DBR.getArticlesSortedByAuthor) connection
    pure (
        case sessionResults of
            Right results -> H . Right . Right $ encode results
            Left sessionError -> case getError sessionError of
                Just (PSQL_INVALID_ROW_COUNT_IN_RESULT_OFFSET_CLAUSE, Just msg) -> if "OFFSET" `isPrefixOf` msg
                    then H . Right . Left $ encode eNegativeOffset
                    else H . Left $ show sessionError
                _ -> H . Left $ show sessionError
        )

getArticlesSortedByCategory :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> OffsetRequest
    -> m (HasqlSessionResults ByteString)
getArticlesSortedByCategory sessionRun connection request = do
    let params = offset (request :: OffsetRequest)
    sessionResults <- sessionRun (Session.statement params DBR.getArticlesSortedByCategory) connection
    pure (
        case sessionResults of
            Right results -> H . Right . Right $ encode results
            Left sessionError -> case getError sessionError of
                Just (PSQL_INVALID_ROW_COUNT_IN_RESULT_OFFSET_CLAUSE, Just msg) -> if "OFFSET" `isPrefixOf` msg
                    then H . Right . Left $ encode eNegativeOffset
                    else H . Left $ show sessionError
                _ -> H . Left $ show sessionError
        )

getArticlesFilteredBy :: MonadIO m =>
    Statement (Text, Maybe Int32) Value
    -> (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticlesByCreationDateRequest
    -> m (HasqlSessionResults ByteString)
getArticlesFilteredBy statement sessionRun connection articlesByCreationDateRequest = do
    let params = (
            pack . showGregorian $ day (articlesByCreationDateRequest :: ArticlesByCreationDateRequest),
            offset (articlesByCreationDateRequest :: ArticlesByCreationDateRequest)
            )
    sessionResults <- sessionRun (Session.statement params statement) connection
    pure (
        case sessionResults of
            Right results -> H . Right . Right $ encode results
            Left sessionError -> case getError sessionError of
                Just (PSQL_INVALID_ROW_COUNT_IN_RESULT_OFFSET_CLAUSE, Just msg) -> if "OFFSET" `isPrefixOf` msg
                    then H . Right . Left $ encode eNegativeOffset
                    else H . Left $ show sessionError
                _ -> H . Left $ show sessionError
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
getCredentials sessionRun connection authRequest = do
    let params = (
            username (authRequest :: AuthRequest),
            password (authRequest :: AuthRequest)
            )
    sessionResults <- sessionRun (Session.statement params DBR.getCredentials) connection
    pure (
        case sessionResults of
            Right results -> H . Right $ Right results
            Left sessionError -> case getErrorCode sessionError of
                Just UnexpectedAmountOfRowsOrUnexpectedNull -> H . Right . Left $ encode eWrongUsernameOrPassword
                _ -> H . Left $ show sessionError
        )
