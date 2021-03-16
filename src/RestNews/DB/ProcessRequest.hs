{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE OverloadedStrings  #-}

module RestNews.DB.ProcessRequest (
    getError,
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
import Data.Bifunctor (bimap)
import Data.ByteString.Lazy.UTF8 (ByteString)
import Data.ByteString.Internal (unpackChars)
import Data.Int (Int32)
import Data.Text (Text, pack)
import Data.Time.Calendar (showGregorian)
import Hasql.Connection (Connection)
import qualified Hasql.Session as Session
import Hasql.Statement (Statement)

import RestNews.Requests.JSON
import qualified RestNews.DB.Request as DBR

-- https://hackage.haskell.org/package/hasql-1.4.4
-- https://github.com/nikita-volkov/hasql-tutorial1


valueToUTFLBS :: Either Session.QueryError Value -> Either String ByteString
valueToUTFLBS = bimap show encode

getError :: Either Session.QueryError resultsType -> Maybe (String, Maybe String)
getError (Left (Session.QueryError _ _ (Session.ResultError (Session.ServerError "22001" _ details _)))) =
    Just ("22001", fmap unpackChars details)

getError (Left (Session.QueryError _ _ (Session.ResultError (Session.ServerError "23503" _ details _)))) =
    Just ("23503", fmap unpackChars details)

getError (Left (Session.QueryError _ _ (Session.ResultError (Session.ServerError "23505" _ details _)))) =
    Just ("23505", fmap unpackChars details)

getError (Left (Session.QueryError _ _ (Session.ResultError (Session.UnexpectedAmountOfRows 0)))) =
    Just ("0", Nothing)

getError (Left (Session.QueryError _ _ (Session.ResultError (Session.RowError 0 Session.UnexpectedNull)))) =
    Just ("0", Nothing)

getError _ = Nothing

getErrorCode :: Either Session.QueryError resultsType -> Maybe String
getErrorCode = fmap fst . getError

createUser :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> CreateUserRequest
    -> m (Either String ByteString, Maybe ByteString)
createUser sessionRun connection createUserRequest = let {
    params = (
        username (createUserRequest :: CreateUserRequest),
        password (createUserRequest :: CreateUserRequest),
        name (createUserRequest :: CreateUserRequest),
        surname createUserRequest,
        avatar createUserRequest
        );
} in do
    sessionResults <- sessionRun (Session.statement params DBR.createUser) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "22001" -> Just "{\"error\": \"name and surname field length must be 80 characters at most\"}"
            Just "23505" -> Just "{\"error\": \"user with this username already exists\"}"
            _ -> Nothing
        )

deleteUser :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> UserIdRequest
    -> m (Either String ByteString, Maybe ByteString)
deleteUser sessionRun connection deleteUserRequest = let {
    params = user_id (deleteUserRequest :: UserIdRequest);
} in do
    sessionResults <- sessionRun (Session.statement params DBR.deleteUser) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "0" -> Just "{\"error\": \"such user does not exist\"}"
            _ -> Nothing
        )

getUser :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> Int32
    -> m (Either String ByteString, Maybe ByteString)
getUser sessionRun connection userId = do
    sessionResults <- sessionRun (Session.statement userId DBR.getUser) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "0" -> Just "{\"error\": \"such user does not exist\"}"
            _ -> Nothing
        )


promoteUserToAuthor :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection -> PromoteUserToAuthorRequest
    -> m (Either String ByteString, Maybe ByteString)
promoteUserToAuthor sessionRun connection promoteUserToAuthorRequest = let {
    params = (
        user_id (promoteUserToAuthorRequest :: PromoteUserToAuthorRequest),
        description (promoteUserToAuthorRequest :: PromoteUserToAuthorRequest));
} in do
    sessionResults <- sessionRun (Session.statement params DBR.promoteUserToAuthor) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "23503" -> Just "{\"error\": \"such user does not exist\"}"
            Just "23505" -> Just "{\"error\": \"such user is already an author\"}"
            _ -> Nothing
        )

editAuthor :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> EditAuthorRequest
    -> m (Either String ByteString, Maybe ByteString)
editAuthor sessionRun connection editAuthorRequest = let {
    params = (
        author_id (editAuthorRequest :: EditAuthorRequest),
        description (editAuthorRequest :: EditAuthorRequest));
} in do
    sessionResults <- sessionRun (Session.statement params DBR.editAuthor) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "22001" -> Just "{\"error\": \"name and surname field length must be 80 characters at most\"}"
            Just "0" -> Just "{\"error\": \"such author does not exist\"}"
            _ -> Nothing
        )

getAuthor :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> AuthorIdRequest
    -> m (Either String ByteString, Maybe ByteString)
getAuthor sessionRun connection authorIdRequest = let {
    params = author_id (authorIdRequest :: AuthorIdRequest);
} in do
    sessionResults <- sessionRun (Session.statement params DBR.getAuthor) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "0" -> Just "{\"error\": \"such author does not exist\"}"
            _ -> Nothing
        )

deleteAuthorRole :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> AuthorIdRequest
    -> m (Either String ByteString, Maybe ByteString)
deleteAuthorRole sessionRun connection authorIdRequest = let {
    params = author_id (authorIdRequest :: AuthorIdRequest);
} in do
    sessionResults <- sessionRun (Session.statement params DBR.deleteAuthorRole) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "0" -> Just "{\"error\": \"such author does not exist\"}"
            _ -> Nothing
        )

createCategory :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> CreateCategoryRequest
    -> m (Either String ByteString, Maybe ByteString)
createCategory sessionRun connection createCategoryRequest = let {
    params = (
        name (createCategoryRequest :: CreateCategoryRequest),
        parent_id (createCategoryRequest :: CreateCategoryRequest)
        );
} in do
    sessionResults <- sessionRun (Session.statement params DBR.createCategory) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "22001" -> Just "{\"error\": \"category name length must be 80 characters at most\"}"
            Just "23503" -> Just "{\"error\": \"parent category does not exist\"}"
            _ -> Nothing
        )

updateCategory :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> UpdateCategoryRequest
    -> m (Either String ByteString, Maybe ByteString)
updateCategory sessionRun connection updateCategoryRequest = let {
    params = (
        category_id (updateCategoryRequest :: UpdateCategoryRequest),
        name (updateCategoryRequest :: UpdateCategoryRequest),
        parent_id (updateCategoryRequest :: UpdateCategoryRequest)
        );
} in do
    sessionResults <- sessionRun (Session.statement params DBR.updateCategory) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "22001" -> Just "{\"error\": \"category name length must be 80 characters at most\"}"
            Just "23503" -> Just "{\"error\": \"parent category does not exist\"}"
            Just "0" -> Just "{\"error\": \"no such category\"}"
            _ -> Nothing
        )

getCategory :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> CategoryIdRequest
    -> m (Either String ByteString, Maybe ByteString)
getCategory sessionRun connection categoryIdRequest = let {
    params = category_id (categoryIdRequest :: CategoryIdRequest);
} in do
    sessionResults <- sessionRun (Session.statement params DBR.getCategory) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "0" -> Just "{\"error\": \"no such category\"}"
            _ -> Nothing
        )

deleteCategory :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> CategoryIdRequest
    -> m (Either String ByteString, Maybe ByteString)
deleteCategory sessionRun connection categoryIdRequest = let {
    params = category_id (categoryIdRequest :: CategoryIdRequest);
} in do
    sessionResults <- sessionRun (Session.statement params DBR.deleteCategory) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "23503" -> Just "{\"error\": \"category is in use\"}"
            Just "0" -> Just "{\"error\": \"no such category\"}"
            _ -> Nothing
        )


createTag :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> CreateTagRequest
    -> m (Either String ByteString, Maybe ByteString)
createTag sessionRun connection createTagRequest = let {
    params = tag_name (createTagRequest :: CreateTagRequest);
} in do
    sessionResults <- sessionRun (Session.statement params DBR.createTag) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "22001" -> Just "{\"error\": \"tag_name length must be 80 characters at most\"}"
            Just "23505" -> Just "{\"error\": \"tag with such name already exists\"}"
            _ -> Nothing
        )

editTag :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> EditTagRequest
    -> m (Either String ByteString, Maybe ByteString)
editTag sessionRun connection editTagRequest = let {
    params = (tag_id (editTagRequest :: EditTagRequest), tag_name (editTagRequest :: EditTagRequest));
} in do
    sessionResults <- sessionRun (Session.statement params DBR.editTag) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "22001" -> Just "{\"error\": \"tag_name length must be 80 characters at most\"}"
            Just "23505" -> Just "{\"error\": \"tag with such name already exists\"}"
            Just "0" -> Just "{\"error\": \"no such tag\"}"
            _ -> Nothing
        )

deleteTag :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> TagIdRequest
    -> m (Either String ByteString, Maybe ByteString)
deleteTag sessionRun connection deleteTagRequest = let {
    params = tag_id (deleteTagRequest :: TagIdRequest);
} in do
    sessionResults <- sessionRun (Session.statement params DBR.deleteTag) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "23503" -> Just "{\"error\": \"tag is referenced by an article\"}"
            Just "0" -> Just "{\"error\": \"no such tag\"}"
            _ -> Nothing
        )

getTag :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> TagIdRequest
    -> m (Either String ByteString, Maybe ByteString)
getTag sessionRun connection getTagRequest = let {
    params = tag_id (getTagRequest :: TagIdRequest);
} in do
    sessionResults <- sessionRun (Session.statement params DBR.getTag) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "0" -> Just "{\"error\": \"no such tag\"}"
            _ -> Nothing
        )


createComment :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> CreateCommentRequest
    -> Int32
    -> m (Either String ByteString, Maybe ByteString)
createComment sessionRun connection createCommentRequest user_id' = let {
    params = (
        article_id (createCommentRequest :: CreateCommentRequest),
        comment_text (createCommentRequest :: CreateCommentRequest),
        user_id'
        );
} in do
    sessionResults <- sessionRun (Session.statement params DBR.createComment) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "23503" -> Just "{\"error\": \"no such article\"}"
            _ -> Nothing
        )

deleteComment :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> CommentIdRequest
    -> Int32
    -> m (Either String ByteString, Maybe ByteString)
deleteComment sessionRun connection deleteCommentRequest user_id' = let {
    params = (
        comment_id (deleteCommentRequest :: CommentIdRequest),
        user_id'
    );
} in do
    sessionResults <- sessionRun (Session.statement params DBR.deleteComment) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "0" -> Just "{\"error\": \"no such comment\"}"
            _ -> Nothing
        )

getArticleComments :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticleCommentsRequest
    -> m (Either String ByteString, Maybe ByteString)
getArticleComments sessionRun connection articleCommentsRequest = let {
    params = (
        article_id (articleCommentsRequest :: ArticleCommentsRequest),
        offset (articleCommentsRequest :: ArticleCommentsRequest)
        );
} in do
    sessionResults <- sessionRun (Session.statement params DBR.getArticleComments) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )


createArticleDraft :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticleDraftRequest
    -> Int32
    -> m (Either String ByteString, Maybe ByteString)
createArticleDraft sessionRun connection articleDraftRequest author_id' = let {
    params = (
        author_id',
        category_id (articleDraftRequest :: ArticleDraftRequest),
        article_title (articleDraftRequest :: ArticleDraftRequest),
        article_content (articleDraftRequest :: ArticleDraftRequest),
        tags (articleDraftRequest :: ArticleDraftRequest),
        main_photo (articleDraftRequest :: ArticleDraftRequest),
        additional_photos (articleDraftRequest :: ArticleDraftRequest)
        );
} in do
    sessionResults <- sessionRun (Session.statement params DBR.createArticleDraft) connection
    pure (
        valueToUTFLBS sessionResults,
        case getError sessionResults of
            Just ("22001", _) -> Just "{\"error\": \"article_title length must be 80 characters at most\"}"
            Just ("23503", details) ->
                let {
                    detailsPrefix = fmap (take 12) details;
                } in case detailsPrefix of
                    Just "Key (tag_id)" -> Just "{\"error\": \"no such tag\"}"
                    Just "Key (categor" -> Just "{\"error\": \"no such category\"}"
                    _ -> error $ show details
            Just ("0", Nothing) -> Just "{\"error\": \"no such article\"}"
            _ -> Nothing
        )

publishArticleDraft :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticleDraftIdRequest
    -> Int32 ->
    m (Either String ByteString, Maybe ByteString)
publishArticleDraft sessionRun connection articleDraftIdRequest author_id' = let {
    params = (
        author_id',
        article_id (articleDraftIdRequest :: ArticleDraftIdRequest)
        );
} in do
    sessionResults <- sessionRun (Session.statement params DBR.publishArticleDraft) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "0" -> Just "{\"error\": \"no such article\"}"
            _ -> Nothing
        )

editArticleDraft :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticleDraftEditRequest
    -> Int32
    -> m (Either String ByteString, Maybe ByteString)
editArticleDraft sessionRun connection articleDraftEditRequest author_id' = let {
    params = (
        author_id',
        article_id (articleDraftEditRequest :: ArticleDraftEditRequest),
        category_id (articleDraftEditRequest :: ArticleDraftEditRequest),
        article_title (articleDraftEditRequest :: ArticleDraftEditRequest),
        article_content (articleDraftEditRequest :: ArticleDraftEditRequest),
        main_photo (articleDraftEditRequest :: ArticleDraftEditRequest),
        additional_photos (articleDraftEditRequest :: ArticleDraftEditRequest),
        tags (articleDraftEditRequest :: ArticleDraftEditRequest)
        );
} in do
    sessionResults <- sessionRun (Session.statement params DBR.editArticleDraft) connection
    pure (
        valueToUTFLBS sessionResults,
        case getError sessionResults of
            Just ("22001", _) -> Just "{\"error\": \"article_title length must be 80 characters at most\"}"
            Just ("23503", details) ->
                let {
                    detailsPrefix = fmap (take 12) details;
                } in case detailsPrefix of
                    Just "Key (tag_id)" -> Just "{\"error\": \"no such tag\"}"
                    Just "Key (categor" -> Just "{\"error\": \"no such category\"}"
                    Just "Key (article" -> Just "{\"error\": \"no such article\"}"
                    _ -> error $ show details
            Just ("0", Nothing) -> Just "{\"error\": \"no such article\"}"
            _ -> Nothing
        )

getArticleDraft :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticleDraftIdRequest
    -> Int32 ->
    m (Either String ByteString, Maybe ByteString)
getArticleDraft sessionRun connection articleDraftIdRequest author_id' = let {
    params = (
        article_id (articleDraftIdRequest :: ArticleDraftIdRequest),
        author_id'
        );
} in do
    sessionResults <- sessionRun (Session.statement params DBR.getArticleDraft) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "0" -> Just "{\"error\": \"no such article\"}"
            _ -> Nothing
        )

deleteArticleDraft :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticleDraftIdRequest
    -> Int32
    -> m (Either String ByteString, Maybe ByteString)
deleteArticleDraft sessionRun connection articleDraftIdRequest author_id' = let {
    params = (
        article_id (articleDraftIdRequest :: ArticleDraftIdRequest),
        author_id'
        );
} in do
    sessionResults <- sessionRun (Session.statement params DBR.deleteArticleDraft) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "0" -> Just "{\"error\": \"no such article\"}"
            _ -> Nothing
        )


getArticlesByCategoryId :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticlesByCategoryIdRequest
    -> m (Either String ByteString, Maybe ByteString)
getArticlesByCategoryId sessionRun connection articlesByCategoryIdRequest = let {
    params = (
        category_id (articlesByCategoryIdRequest :: ArticlesByCategoryIdRequest),
        offset (articlesByCategoryIdRequest :: ArticlesByCategoryIdRequest)
        );
} in do
    sessionResults <- sessionRun (Session.statement params DBR.getArticlesByCategoryId) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )

getArticlesByTagId :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> TagIdRequestWithOffset
    -> m (Either String ByteString, Maybe ByteString)
getArticlesByTagId sessionRun connection tagIdRequestWithOffset = let {
    params = (
        tag_id (tagIdRequestWithOffset :: TagIdRequestWithOffset),
        offset (tagIdRequestWithOffset :: TagIdRequestWithOffset)
        );
} in do
    sessionResults <- sessionRun (Session.statement params DBR.getArticlesByTagId) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )


getArticlesByAnyTagId :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticlesByTagIdListRequest
    -> m (Either String ByteString, Maybe ByteString)
getArticlesByAnyTagId sessionRun connection tagIdsRequest = let {
    params = (
        tags_ids (tagIdsRequest :: ArticlesByTagIdListRequest),
        offset (tagIdsRequest :: ArticlesByTagIdListRequest)
        );
} in do
    sessionResults <- sessionRun (Session.statement params DBR.getArticlesByAnyTagId) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )

getArticlesByAllTagId :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticlesByTagIdListRequest
    -> m (Either String ByteString, Maybe ByteString)
getArticlesByAllTagId sessionRun connection tagIdsRequest = let {
    params = (
        tags_ids (tagIdsRequest :: ArticlesByTagIdListRequest),
        offset (tagIdsRequest :: ArticlesByTagIdListRequest)
        );
} in do
    sessionResults <- sessionRun (Session.statement params DBR.getArticlesByAllTagId) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )

getArticlesByTitlePart :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticlesByTitlePartRequest
    -> m (Either String ByteString, Maybe ByteString)
getArticlesByTitlePart sessionRun connection substringRequest = let {
    params = (
        title_substring (substringRequest :: ArticlesByTitlePartRequest),
        offset (substringRequest :: ArticlesByTitlePartRequest)
        );
} in do
    sessionResults <- sessionRun (Session.statement params DBR.getArticlesByTitlePart) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )

getArticlesByContentPart :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticlesByContentPartRequest
    -> m (Either String ByteString, Maybe ByteString)
getArticlesByContentPart sessionRun connection substringRequest = let {
    params = (
        content_substring (substringRequest :: ArticlesByContentPartRequest),
        offset (substringRequest :: ArticlesByContentPartRequest)
        );
} in do
    sessionResults <- sessionRun (Session.statement params DBR.getArticlesByContentPart) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )

getArticlesByAuthorNamePart :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticlesByAuthorNamePartRequest
    -> m (Either String ByteString, Maybe ByteString)
getArticlesByAuthorNamePart sessionRun connection substringRequest = let {
    params = (
        author_name_substring (substringRequest :: ArticlesByAuthorNamePartRequest),
        offset (substringRequest :: ArticlesByAuthorNamePartRequest)
        );
} in do
    sessionResults <- sessionRun (Session.statement params DBR.getArticlesByAuthorNamePart) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )

getArticlesSortedByPhotosNumber :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> OffsetRequest
    -> m (Either String ByteString, Maybe ByteString)
getArticlesSortedByPhotosNumber sessionRun connection request = let {
    params = (
        offset (request :: OffsetRequest)
        );
} in do
    sessionResults <- sessionRun (Session.statement params DBR.getArticlesSortedByPhotosNumber) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )

getArticlesSortedByCreationDate :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> OffsetRequest
    -> m (Either String ByteString, Maybe ByteString)
getArticlesSortedByCreationDate sessionRun connection request = let {
    params = (
        offset (request :: OffsetRequest)
        );
} in do
    sessionResults <- sessionRun (Session.statement params DBR.getArticlesSortedByCreationDate) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )

getArticlesSortedByAuthor :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> OffsetRequest
    -> m (Either String ByteString, Maybe ByteString)
getArticlesSortedByAuthor sessionRun connection request = let {
    params = (
        offset (request :: OffsetRequest)
        );
} in do
    sessionResults <- sessionRun (Session.statement params DBR.getArticlesSortedByAuthor) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )

getArticlesSortedByCategory :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> OffsetRequest
    -> m (Either String ByteString, Maybe ByteString)
getArticlesSortedByCategory sessionRun connection request = let {
    params = (
        offset (request :: OffsetRequest)
        );
} in do
    sessionResults <- sessionRun (Session.statement params DBR.getArticlesSortedByCategory) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )

getArticlesFilteredBy :: MonadIO m =>
    Statement (Text, Maybe Int32) Value
    -> (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticlesByCreationDateRequest
    -> m (Either String ByteString, Maybe ByteString)
getArticlesFilteredBy statement sessionRun connection articlesByCreationDateRequest = let {
    params = (
        pack . showGregorian $ day (articlesByCreationDateRequest :: ArticlesByCreationDateRequest),
        offset (articlesByCreationDateRequest :: ArticlesByCreationDateRequest)
        );
} in do
    sessionResults <- sessionRun (Session.statement params statement) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )

getArticlesFilteredByCreationDate :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticlesByCreationDateRequest
    -> m (Either String ByteString, Maybe ByteString)
getArticlesFilteredByCreationDate = getArticlesFilteredBy DBR.getArticlesFilteredByCreationDate

getArticlesCreatedBeforeDate :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticlesByCreationDateRequest
    -> m (Either String ByteString, Maybe ByteString)
getArticlesCreatedBeforeDate = getArticlesFilteredBy DBR.getArticlesCreatedBeforeDate

getArticlesCreatedAfterDate :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> ArticlesByCreationDateRequest ->
    m (Either String ByteString, Maybe ByteString)
getArticlesCreatedAfterDate = getArticlesFilteredBy DBR.getArticlesCreatedAfterDate


getCredentials :: MonadIO m => 
    (Session.Session (Int32, Bool, Int32) -> Connection -> m (Either Session.QueryError (Int32, Bool, Int32)))
    -> Connection -> AuthRequest -> m (Either String (Int32, Bool, Int32), Maybe ByteString)
getCredentials sessionRun connection authRequest = let {
    params = (
        username (authRequest :: AuthRequest),
        password (authRequest :: AuthRequest)
        );
} in do
    sessionResults <- sessionRun (Session.statement params DBR.getCredentials) connection
    pure (
        bimap show id sessionResults,
        case getErrorCode sessionResults of
            Just "0" -> Just "wrong username/password"
            _ -> Nothing
        )
