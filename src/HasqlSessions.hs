{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE OverloadedStrings  #-}

module HasqlSessions (
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

import Control.Monad.IO.Class
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
import qualified Util

import AesonDefinitions
import qualified HasqlStatements as HST

-- https://hackage.haskell.org/package/hasql-1.4.4
-- https://github.com/nikita-volkov/hasql-tutorial1


valueToUTFLBS :: Either Session.QueryError Value -> Either String ByteString
valueToUTFLBS = bimap show encode

getError :: Either Session.QueryError resultsType -> Maybe (String, Maybe String)
{-
    Left (
        QueryError
            "SELECT edit_article_draft($1 :: int4, $2 :: int4, $3 :: int4, $4 :: text, $5 :: text, $6 :: text, $7 :: text[], $8 :: int4[]) :: json"
            ["1","9","2","\"PATCHEDttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt\"","\"PATCHED\"","\"fs\"","[\"9\", \"2\"]","[1, 2]"]
            (
                ResultError
                    (
                        ServerError
                            "22001"
                            "value too long for type character varying(80)"
                            Nothing
                            Nothing
                    )
            )
    )
-}
getError (Left (Session.QueryError _ _ (Session.ResultError (Session.ServerError "22001" _ details _)))) =
    Just ("22001", fmap unpackChars details)
{-
Left (
    QueryError
        "WITH delete_results AS (DELETE FROM categories WHERE category_id = $1 :: int4 RETURNING *) SELECT CASE WHEN count(delete_results) = 0 THEN json_build_object('error', 'no such category') ELSE json_build_object('results', 'ook') END :: json FROM delete_results"
        ["1"]
        (
            ResultError (
                ServerError
                    "23503"
                    "update or delete on table \"categories\" violates foreign key constraint \"categories_parent_id_fkey\" on table \"categories\""
                    (Just "Key (category_id)=(1) is still referenced from table \"categories\".")
                    Nothing
            )
        )
)
-}
getError (Left (Session.QueryError _ _ (Session.ResultError (Session.ServerError "23503" _ details _)))) =
    Just ("23503", fmap unpackChars details)
{-
Left (
    QueryError
        "INSERT INTO users (username, password, name, surname, avatar, is_admin) VALUES ($1 :: text, crypt($2 :: text, gen_salt('bf', 8)), $3 :: text, $4 :: text, $5 :: text, FALSE) RETURNING json_build_object('user_id', user_id, 'name', name, 'surname', surname, 'avatar', avatar, 'creation_date', creation_date, 'is_admin', is_admin) :: json"
        ["\"asdq\"","\"check, indeed\"","\"name\"","\"surname\"","\"asd\""]
        (
            ResultError (
                ServerError
                    "23505"
                    "duplicate key value violates unique constraint \"users_username_key\""
                    (Just "Key (username)=(asdq) already exists.")
                    Nothing
            )
        )
)
-}
getError (Left (Session.QueryError _ _ (Session.ResultError (Session.ServerError "23505" _ details _)))) =
    Just ("23505", fmap unpackChars details)
{-
Left (
    QueryError
        "SELECT users.user_id :: int4, users.is_admin :: bool, COALESCE (authors.author_id, 0) :: int4 FROM users LEFT JOIN authors ON authors.user_id = users.user_id, (SELECT user_id FROM users WHERE username = lower($1 :: text) AND password = crypt($2 :: text, password)) AS matched_user WHERE users.user_id = matched_user.user_id"
        ["\"usasdername5\"","\"12345\""]
        (
            ResultError (
                UnexpectedAmountOfRows 0
            )
        )
)
-}
getError (Left (Session.QueryError _ _ (Session.ResultError (Session.UnexpectedAmountOfRows 0)))) =
    Just ("0", Nothing)
-- (Nothing,Left (QueryError "SELECT json_agg(users.*) :: json FROM users WHERE user_id = $1 :: int4" ["20"] (ResultError (RowError 0 UnexpectedNull))))
getError (Left (Session.QueryError _ _ (Session.ResultError (Session.RowError 0 Session.UnexpectedNull)))) =
    Just ("0", Nothing)
getError _ = Nothing

getErrorCode :: Either Session.QueryError resultsType -> Maybe String
getErrorCode = fmap fst . getError


createUser' :: MonadIO m =>
    (Session.Session Value -> Connection -> m (Either Session.QueryError Value))
    -> Connection
    -> CreateUserRequest
    -> m (Either String ByteString, Maybe ByteString)
createUser' sessionRun connection createUserRequest = let {
    params = (
        username (createUserRequest :: CreateUserRequest),
        password (createUserRequest :: CreateUserRequest),
        name (createUserRequest :: CreateUserRequest),
        surname createUserRequest,
        avatar createUserRequest
        );
} in do
    sessionResults <- sessionRun (Session.statement params HST.createUser) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "22001" -> Just "{\"error\": \"name and surname field length must be 80 characters at most\"}"
            Just "23505" -> Just "{\"error\": \"user with this username already exists\"}"
            _ -> Nothing
        )

createUser :: MonadIO m =>
    (Connection -> CreateUserRequest -> m (Either String ByteString, Maybe ByteString))
createUser = createUser' $ liftIO Util.∘∘ Session.run

deleteUser :: MonadIO m => Connection -> UserIdRequest -> m (Either String ByteString, Maybe ByteString)
deleteUser connection deleteUserRequest = let {
    params = user_id (deleteUserRequest :: UserIdRequest);
} in do
    sessionResults <- liftIO $ Session.run (Session.statement params HST.deleteUser) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "0" -> Just "{\"error\": \"such user does not exist\"}"
            _ -> Nothing
        )

getUser :: MonadIO m => Connection -> Int32 -> m (Either String ByteString, Maybe ByteString)
getUser connection userId = do
    sessionResults <- liftIO $ Session.run (Session.statement userId HST.getUser) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "0" -> Just "{\"error\": \"such user does not exist\"}"
            _ -> Nothing
        )


promoteUserToAuthor :: MonadIO m => Connection -> PromoteUserToAuthorRequest
    -> m (Either String ByteString, Maybe ByteString)
promoteUserToAuthor connection promoteUserToAuthorRequest = let {
    params = (
        user_id (promoteUserToAuthorRequest :: PromoteUserToAuthorRequest),
        description (promoteUserToAuthorRequest :: PromoteUserToAuthorRequest));
} in do
    sessionResults <- liftIO $ Session.run (Session.statement params HST.promoteUserToAuthor) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "23503" -> Just "{\"error\": \"such user does not exist\"}"
            Just "23505" -> Just "{\"error\": \"such user is already an author\"}"
            _ -> Nothing
        )

editAuthor :: MonadIO m => Connection -> EditAuthorRequest -> m (Either String ByteString, Maybe ByteString)
editAuthor connection editAuthorRequest = let {
    params = (
        author_id (editAuthorRequest :: EditAuthorRequest),
        description (editAuthorRequest :: EditAuthorRequest));
} in do
    sessionResults <- liftIO $ Session.run (Session.statement params HST.editAuthor) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "22001" -> Just "{\"error\": \"name and surname field length must be 80 characters at most\"}"
            Just "0" -> Just "{\"error\": \"such author does not exist\"}"
            _ -> Nothing
        )

getAuthor :: MonadIO m => Connection -> AuthorIdRequest -> m (Either String ByteString, Maybe ByteString)
getAuthor connection authorIdRequest = let {
    params = author_id (authorIdRequest :: AuthorIdRequest);
} in do
    sessionResults <- liftIO $ Session.run (Session.statement params HST.getAuthor) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "0" -> Just "{\"error\": \"such author does not exist\"}"
            _ -> Nothing
        )

deleteAuthorRole :: MonadIO m => Connection -> AuthorIdRequest
    -> m (Either String ByteString, Maybe ByteString)
deleteAuthorRole connection authorIdRequest = let {
    params = author_id (authorIdRequest :: AuthorIdRequest);
} in do
    sessionResults <- liftIO $ Session.run (Session.statement params HST.deleteAuthorRole) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "0" -> Just "{\"error\": \"such author does not exist\"}"
            _ -> Nothing
        )

createCategory :: MonadIO m => Connection -> CreateCategoryRequest -> m (Either String ByteString, Maybe ByteString)
createCategory connection createCategoryRequest = let {
    params = (
        name (createCategoryRequest :: CreateCategoryRequest),
        parent_id (createCategoryRequest :: CreateCategoryRequest)
        );
} in do
    sessionResults <- liftIO $ Session.run (Session.statement params HST.createCategory) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "22001" -> Just "{\"error\": \"category name length must be 80 characters at most\"}"
            Just "23503" -> Just "{\"error\": \"parent category does not exist\"}"
            _ -> Nothing
        )

updateCategory :: MonadIO m => Connection -> UpdateCategoryRequest -> m (Either String ByteString, Maybe ByteString)
updateCategory connection updateCategoryRequest = let {
    params = (
        category_id (updateCategoryRequest :: UpdateCategoryRequest),
        name (updateCategoryRequest :: UpdateCategoryRequest),
        parent_id (updateCategoryRequest :: UpdateCategoryRequest)
        );
} in do
    sessionResults <- liftIO $ Session.run (Session.statement params HST.updateCategory) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "22001" -> Just "{\"error\": \"category name length must be 80 characters at most\"}"
            Just "23503" -> Just "{\"error\": \"parent category does not exist\"}"
            Just "0" -> Just "{\"error\": \"no such category\"}"
            _ -> Nothing
        )

getCategory :: MonadIO m => Connection -> CategoryIdRequest -> m (Either String ByteString, Maybe ByteString)
getCategory connection categoryIdRequest = let {
    params = category_id (categoryIdRequest :: CategoryIdRequest);
} in do
    sessionResults <- liftIO $ Session.run (Session.statement params HST.getCategory) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "0" -> Just "{\"error\": \"no such category\"}"
            _ -> Nothing
        )

deleteCategory :: MonadIO m => Connection -> CategoryIdRequest -> m (Either String ByteString, Maybe ByteString)
deleteCategory connection categoryIdRequest = let {
    params = category_id (categoryIdRequest :: CategoryIdRequest);
} in do
    sessionResults <- liftIO $ Session.run (Session.statement params HST.deleteCategory) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "23503" -> Just "{\"error\": \"category is in use\"}"
            Just "0" -> Just "{\"error\": \"no such category\"}"
            _ -> Nothing
        )


createTag :: MonadIO m => Connection -> CreateTagRequest -> m (Either String ByteString, Maybe ByteString)
createTag connection createTagRequest = let {
    params = tag_name (createTagRequest :: CreateTagRequest);
} in do
    sessionResults <- liftIO $ Session.run (Session.statement params HST.createTag) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "22001" -> Just "{\"error\": \"tag_name length must be 80 characters at most\"}"
            Just "23505" -> Just "{\"error\": \"tag with such name already exists\"}"
            _ -> Nothing
        )

editTag :: MonadIO m => Connection -> EditTagRequest -> m (Either String ByteString, Maybe ByteString)
editTag connection editTagRequest = let {
    params = (tag_id (editTagRequest :: EditTagRequest), tag_name (editTagRequest :: EditTagRequest));
} in do
    sessionResults <- liftIO $ Session.run (Session.statement params HST.editTag) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "22001" -> Just "{\"error\": \"tag_name length must be 80 characters at most\"}"
            Just "23505" -> Just "{\"error\": \"tag with such name already exists\"}"
            Just "0" -> Just "{\"error\": \"no such tag\"}"
            _ -> Nothing
        )

deleteTag :: MonadIO m => Connection -> TagIdRequest -> m (Either String ByteString, Maybe ByteString)
deleteTag connection deleteTagRequest = let {
    params = tag_id (deleteTagRequest :: TagIdRequest);
} in do
    sessionResults <- liftIO $ Session.run (Session.statement params HST.deleteTag) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "23503" -> Just "{\"error\": \"tag is referenced by an article\"}"
            Just "0" -> Just "{\"error\": \"no such tag\"}"
            _ -> Nothing
        )

getTag :: MonadIO m => Connection -> TagIdRequest -> m (Either String ByteString, Maybe ByteString)
getTag connection getTagRequest = let {
    params = tag_id (getTagRequest :: TagIdRequest);
} in do
    sessionResults <- liftIO $ Session.run (Session.statement params HST.getTag) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "0" -> Just "{\"error\": \"no such tag\"}"
            _ -> Nothing
        )


createComment :: MonadIO m => Connection -> CreateCommentRequest -> Int32 -> m (Either String ByteString, Maybe ByteString)
createComment connection createCommentRequest user_id' = let {
    params = (
        article_id (createCommentRequest :: CreateCommentRequest),
        comment_text (createCommentRequest :: CreateCommentRequest),
        user_id'
        );
} in do
    sessionResults <- liftIO $ Session.run (Session.statement params HST.createComment) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "23503" -> Just "{\"error\": \"no such article\"}"
            _ -> Nothing
        )

deleteComment :: MonadIO m => Connection -> CommentIdRequest -> Int32 -> m (Either String ByteString, Maybe ByteString)
deleteComment connection deleteCommentRequest user_id' = let {
    params = (
        comment_id (deleteCommentRequest :: CommentIdRequest),
        user_id'
    );
} in do
    sessionResults <- liftIO $ Session.run (Session.statement params HST.deleteComment) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "0" -> Just "{\"error\": \"no such comment\"}"
            _ -> Nothing
        )

getArticleComments :: MonadIO m => Connection -> ArticleCommentsRequest -> m (Either String ByteString, Maybe ByteString)
getArticleComments connection articleCommentsRequest = let {
    params = (
        article_id (articleCommentsRequest :: ArticleCommentsRequest),
        offset (articleCommentsRequest :: ArticleCommentsRequest)
        );
} in do
    sessionResults <- liftIO $ Session.run (Session.statement params HST.getArticleComments) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )


createArticleDraft :: MonadIO m => Connection -> ArticleDraftRequest -> Int32 -> m (Either String ByteString, Maybe ByteString)
createArticleDraft connection articleDraftRequest author_id' = let {
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
    sessionResults <- liftIO $ Session.run (Session.statement params HST.createArticleDraft) connection
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

publishArticleDraft :: MonadIO m => Connection -> ArticleDraftIdRequest -> Int32 -> m (Either String ByteString, Maybe ByteString)
publishArticleDraft connection articleDraftIdRequest author_id' = let {
    params = (
        author_id',
        article_id (articleDraftIdRequest :: ArticleDraftIdRequest)
        );
} in do
    sessionResults <- liftIO $ Session.run (Session.statement params HST.publishArticleDraft) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "0" -> Just "{\"error\": \"no such article\"}"
            _ -> Nothing
        )

editArticleDraft :: MonadIO m => Connection -> ArticleDraftEditRequest -> Int32 -> m (Either String ByteString, Maybe ByteString)
editArticleDraft connection articleDraftEditRequest author_id' = let {
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
    sessionResults <- liftIO $ Session.run (Session.statement params HST.editArticleDraft) connection
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

getArticleDraft :: MonadIO m => Connection -> ArticleDraftIdRequest -> Int32 -> m (Either String ByteString, Maybe ByteString)
getArticleDraft connection articleDraftIdRequest author_id' = let {
    params = (
        article_id (articleDraftIdRequest :: ArticleDraftIdRequest),
        author_id'
        );
} in do
    sessionResults <- liftIO $ Session.run (Session.statement params HST.getArticleDraft) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "0" -> Just "{\"error\": \"no such article\"}"
            _ -> Nothing
        )

deleteArticleDraft :: MonadIO m => Connection -> ArticleDraftIdRequest -> Int32 -> m (Either String ByteString, Maybe ByteString)
deleteArticleDraft connection articleDraftIdRequest author_id' = let {
    params = (
        article_id (articleDraftIdRequest :: ArticleDraftIdRequest),
        author_id'
        );
} in do
    sessionResults <- liftIO $ Session.run (Session.statement params HST.deleteArticleDraft) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "0" -> Just "{\"error\": \"no such article\"}"
            _ -> Nothing
        )


getArticlesByCategoryId :: MonadIO m => Connection -> ArticlesByCategoryIdRequest -> m (Either String ByteString, Maybe ByteString)
getArticlesByCategoryId connection articlesByCategoryIdRequest = let {
    params = (
        category_id (articlesByCategoryIdRequest :: ArticlesByCategoryIdRequest),
        offset (articlesByCategoryIdRequest :: ArticlesByCategoryIdRequest)
        );
} in do
    sessionResults <- liftIO $ Session.run (Session.statement params HST.getArticlesByCategoryId) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )

getArticlesByTagId :: MonadIO m => Connection -> TagIdRequestWithOffset -> m (Either String ByteString, Maybe ByteString)
getArticlesByTagId connection tagIdRequestWithOffset = let {
    params = (
        tag_id (tagIdRequestWithOffset :: TagIdRequestWithOffset),
        offset (tagIdRequestWithOffset :: TagIdRequestWithOffset)
        );
} in do
    sessionResults <- liftIO $ Session.run (Session.statement params HST.getArticlesByTagId) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )


getArticlesByAnyTagId :: MonadIO m => Connection -> ArticlesByTagIdListRequest -> m (Either String ByteString, Maybe ByteString)
getArticlesByAnyTagId connection tagIdsRequest = let {
    params = (
        tags_ids (tagIdsRequest :: ArticlesByTagIdListRequest),
        offset (tagIdsRequest :: ArticlesByTagIdListRequest)
        );
} in do
    sessionResults <- liftIO $ Session.run (Session.statement params HST.getArticlesByAnyTagId) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )

getArticlesByAllTagId :: MonadIO m => Connection -> ArticlesByTagIdListRequest -> m (Either String ByteString, Maybe ByteString)
getArticlesByAllTagId connection tagIdsRequest = let {
    params = (
        tags_ids (tagIdsRequest :: ArticlesByTagIdListRequest),
        offset (tagIdsRequest :: ArticlesByTagIdListRequest)
        );
} in do
    sessionResults <- liftIO $ Session.run (Session.statement params HST.getArticlesByAllTagId) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )

getArticlesByTitlePart :: MonadIO m => Connection -> ArticlesByTitlePartRequest -> m (Either String ByteString, Maybe ByteString)
getArticlesByTitlePart connection substringRequest = let {
    params = (
        title_substring (substringRequest :: ArticlesByTitlePartRequest),
        offset (substringRequest :: ArticlesByTitlePartRequest)
        );
} in do
    sessionResults <- liftIO $ Session.run (Session.statement params HST.getArticlesByTitlePart) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )

getArticlesByContentPart :: MonadIO m => Connection -> ArticlesByContentPartRequest -> m (Either String ByteString, Maybe ByteString)
getArticlesByContentPart connection substringRequest = let {
    params = (
        content_substring (substringRequest :: ArticlesByContentPartRequest),
        offset (substringRequest :: ArticlesByContentPartRequest)
        );
} in do
    sessionResults <- liftIO $ Session.run (Session.statement params HST.getArticlesByContentPart) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )

getArticlesByAuthorNamePart :: MonadIO m => Connection -> ArticlesByAuthorNamePartRequest -> m (Either String ByteString, Maybe ByteString)
getArticlesByAuthorNamePart connection substringRequest = let {
    params = (
        author_name_substring (substringRequest :: ArticlesByAuthorNamePartRequest),
        offset (substringRequest :: ArticlesByAuthorNamePartRequest)
        );
} in do
    sessionResults <- liftIO $ Session.run (Session.statement params HST.getArticlesByAuthorNamePart) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )

getArticlesSortedByPhotosNumber :: MonadIO m => Connection -> OffsetRequest -> m (Either String ByteString, Maybe ByteString)
getArticlesSortedByPhotosNumber connection request = let {
    params = (
        offset (request :: OffsetRequest)
        );
} in do
    sessionResults <- liftIO $ Session.run (Session.statement params HST.getArticlesSortedByPhotosNumber) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )

getArticlesSortedByCreationDate :: MonadIO m => Connection -> OffsetRequest -> m (Either String ByteString, Maybe ByteString)
getArticlesSortedByCreationDate connection request = let {
    params = (
        offset (request :: OffsetRequest)
        );
} in do
    sessionResults <- liftIO $ Session.run (Session.statement params HST.getArticlesSortedByCreationDate) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )

getArticlesSortedByAuthor :: MonadIO m => Connection -> OffsetRequest -> m (Either String ByteString, Maybe ByteString)
getArticlesSortedByAuthor connection request = let {
    params = (
        offset (request :: OffsetRequest)
        );
} in do
    sessionResults <- liftIO $ Session.run (Session.statement params HST.getArticlesSortedByAuthor) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )

getArticlesSortedByCategory :: MonadIO m => Connection -> OffsetRequest -> m (Either String ByteString, Maybe ByteString)
getArticlesSortedByCategory connection request = let {
    params = (
        offset (request :: OffsetRequest)
        );
} in do
    sessionResults <- liftIO $ Session.run (Session.statement params HST.getArticlesSortedByCategory) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )

getArticlesFilteredBy :: MonadIO m => Statement (Text, Maybe Int32) Value -> Connection -> ArticlesByCreationDateRequest
    -> m (Either String ByteString, Maybe ByteString)
getArticlesFilteredBy statement connection articlesByCreationDateRequest = let {
    params = (
        pack . showGregorian $ day (articlesByCreationDateRequest :: ArticlesByCreationDateRequest),
        offset (articlesByCreationDateRequest :: ArticlesByCreationDateRequest)
        );
} in do
    sessionResults <- liftIO $ Session.run (Session.statement params statement) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )

getArticlesFilteredByCreationDate :: MonadIO m => Connection -> ArticlesByCreationDateRequest
    -> m (Either String ByteString, Maybe ByteString)
getArticlesFilteredByCreationDate = getArticlesFilteredBy HST.getArticlesFilteredByCreationDate

getArticlesCreatedBeforeDate :: MonadIO m => Connection -> ArticlesByCreationDateRequest
    -> m (Either String ByteString, Maybe ByteString)
getArticlesCreatedBeforeDate = getArticlesFilteredBy HST.getArticlesCreatedBeforeDate

getArticlesCreatedAfterDate :: MonadIO m => Connection -> ArticlesByCreationDateRequest -> m (Either String ByteString, Maybe ByteString)
getArticlesCreatedAfterDate = getArticlesFilteredBy HST.getArticlesCreatedAfterDate


getCredentials :: MonadIO m => Connection -> AuthRequest -> m (Either String (Int32, Bool, Int32), Maybe ByteString)
getCredentials connection authRequest = let {
    params = (
        username (authRequest :: AuthRequest),
        password (authRequest :: AuthRequest)
        );
} in do
    sessionResults <- liftIO $ Session.run (Session.statement params HST.getCredentials) connection
    pure (
        bimap show id sessionResults,
        case getErrorCode sessionResults of
            Just "0" -> Just "wrong username/password"
            _ -> Nothing
        )
