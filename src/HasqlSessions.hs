{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE OverloadedStrings  #-}

module HasqlSessions (
    valueToUTFLBS',
    getConnection,
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

import Data.Aeson (Value, encode)
import Data.Bifunctor (bimap)
import Data.ByteString.Lazy.UTF8 (ByteString)
import Data.ByteString.Internal (unpackChars)
import Data.Int (Int32)
import Data.Text (Text, pack)
--import Data.Text.IO (putStrLn)
import Data.Time.Calendar (showGregorian)
import Hasql.Connection (Connection, ConnectionError, Settings, acquire, settings)
import qualified Hasql.Session as Session
import Hasql.Statement (Statement)

import AesonDefinitions
import qualified HasqlStatements as HST

-- https://hackage.haskell.org/package/hasql-1.4.4
-- https://github.com/nikita-volkov/hasql-tutorial1


valueToUTFLBS' :: Either Session.QueryError Value -> Either ByteString ByteString
valueToUTFLBS' = bimap (encode . show) encode

valueToUTFLBS :: Either Session.QueryError Value -> Either Session.QueryError ByteString
valueToUTFLBS = fmap encode

connectionSettings :: Settings
connectionSettings = settings "localhost" 5431 "rest-news-user" "rest" "rest-news-db";

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

-- params and statement type are different
--runSession params statement =
--    Right connection <- acquire connectionSettings
--    sessionResults <- Session.run (Session.statement params statement) connection

getConnection :: IO (Either ConnectionError Connection)
getConnection = acquire connectionSettings

getErrorCode :: Either Session.QueryError resultsType -> Maybe String
getErrorCode = fmap fst . getError

createUser :: Connection -> CreateUserRequest -> IO (Either ByteString ByteString, Maybe ByteString)
createUser connection createUserRequest = let {
    params = (
        username (createUserRequest :: CreateUserRequest),
        password (createUserRequest :: CreateUserRequest),
        name (createUserRequest :: CreateUserRequest),
        surname createUserRequest,
        avatar createUserRequest
        );
} in do
    sessionResults <- Session.run (Session.statement params HST.createUser) connection
    pure (
        valueToUTFLBS' sessionResults,
        case getErrorCode sessionResults of
            Just "22001" -> Just "{\"error\": \"name and surname field length must be 80 characters at most\"}"
            Just "23505" -> Just "{\"error\": \"user with this username already exists\"}"
            _ -> Nothing
        )

deleteUser :: Connection -> UserIdRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
deleteUser connection deleteUserRequest = let {
    params = user_id (deleteUserRequest :: UserIdRequest);
} in do
    sessionResults <- Session.run (Session.statement params HST.deleteUser) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "0" -> Just "{\"error\": \"such user does not exist\"}"
            _ -> Nothing
        )

getUser :: Connection -> Int32 -> IO (Either ByteString ByteString, Maybe ByteString)
getUser connection userId = do
    sessionResults <- Session.run (Session.statement userId HST.getUser) connection
    pure (
        valueToUTFLBS' sessionResults,
        case getErrorCode sessionResults of
            Just "0" -> Just "{\"error\": \"such user does not exist\"}"
            _ -> Nothing
        )


promoteUserToAuthor :: Connection -> PromoteUserToAuthorRequest
    -> IO (Either Session.QueryError ByteString, Maybe ByteString)
promoteUserToAuthor connection promoteUserToAuthorRequest = let {
    params = (
        user_id (promoteUserToAuthorRequest :: PromoteUserToAuthorRequest),
        description (promoteUserToAuthorRequest :: PromoteUserToAuthorRequest));
} in do
    sessionResults <- Session.run (Session.statement params HST.promoteUserToAuthor) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "23503" -> Just "{\"error\": \"such user does not exist\"}"
            Just "23505" -> Just "{\"error\": \"such user is already an author\"}"
            _ -> Nothing
        )

editAuthor :: Connection -> EditAuthorRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
editAuthor connection editAuthorRequest = let {
    params = (
        author_id (editAuthorRequest :: EditAuthorRequest),
        description (editAuthorRequest :: EditAuthorRequest));
} in do
    sessionResults <- Session.run (Session.statement params HST.editAuthor) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "22001" -> Just "{\"error\": \"name and surname field length must be 80 characters at most\"}"
            Just "0" -> Just "{\"error\": \"such author does not exist\"}"
            _ -> Nothing
        )

getAuthor :: Connection -> AuthorIdRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
getAuthor connection authorIdRequest = let {
    params = author_id (authorIdRequest :: AuthorIdRequest);
} in do
    sessionResults <- Session.run (Session.statement params HST.getAuthor) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "0" -> Just "{\"error\": \"such author does not exist\"}"
            _ -> Nothing
        )

deleteAuthorRole :: Connection -> AuthorIdRequest
    -> IO (Either Session.QueryError ByteString, Maybe ByteString)
deleteAuthorRole connection authorIdRequest = let {
    params = author_id (authorIdRequest :: AuthorIdRequest);
} in do
    sessionResults <- Session.run (Session.statement params HST.deleteAuthorRole) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "0" -> Just "{\"error\": \"such author does not exist\"}"
            _ -> Nothing
        )

createCategory :: Connection -> CreateCategoryRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
createCategory connection createCategoryRequest = let {
    params = (
        name (createCategoryRequest :: CreateCategoryRequest),
        parent_id (createCategoryRequest :: CreateCategoryRequest)
        );
} in do
    sessionResults <- Session.run (Session.statement params HST.createCategory) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "22001" -> Just "{\"error\": \"category name length must be 80 characters at most\"}"
            Just "23503" -> Just "{\"error\": \"parent category does not exist\"}"
            _ -> Nothing
        )

updateCategory :: Connection -> UpdateCategoryRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
updateCategory connection updateCategoryRequest = let {
    params = (
        category_id (updateCategoryRequest :: UpdateCategoryRequest),
        name (updateCategoryRequest :: UpdateCategoryRequest),
        parent_id (updateCategoryRequest :: UpdateCategoryRequest)
        );
} in do
    sessionResults <- Session.run (Session.statement params HST.updateCategory) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "22001" -> Just "{\"error\": \"category name length must be 80 characters at most\"}"
            Just "23503" -> Just "{\"error\": \"parent category does not exist\"}"
            Just "0" -> Just "{\"error\": \"no such category\"}"
            _ -> Nothing
        )

getCategory :: Connection -> CategoryIdRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
getCategory connection categoryIdRequest = let {
    params = category_id (categoryIdRequest :: CategoryIdRequest);
} in do
    sessionResults <- Session.run (Session.statement params HST.getCategory) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "0" -> Just "{\"error\": \"no such category\"}"
            _ -> Nothing
        )

deleteCategory :: Connection -> CategoryIdRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
deleteCategory connection categoryIdRequest = let {
    params = category_id (categoryIdRequest :: CategoryIdRequest);
} in do
    sessionResults <- Session.run (Session.statement params HST.deleteCategory) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "23503" -> Just "{\"error\": \"category is in use\"}"
            Just "0" -> Just "{\"error\": \"no such category\"}"
            _ -> Nothing
        )


createTag :: Connection -> CreateTagRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
createTag connection createTagRequest = let {
    params = tag_name (createTagRequest :: CreateTagRequest);
} in do
    sessionResults <- Session.run (Session.statement params HST.createTag) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "22001" -> Just "{\"error\": \"tag_name length must be 80 characters at most\"}"
            Just "23505" -> Just "{\"error\": \"tag with such name already exists\"}"
            _ -> Nothing
        )

editTag :: Connection -> EditTagRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
editTag connection editTagRequest = let {
    params = (tag_id (editTagRequest :: EditTagRequest), tag_name (editTagRequest :: EditTagRequest));
} in do
    sessionResults <- Session.run (Session.statement params HST.editTag) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "22001" -> Just "{\"error\": \"tag_name length must be 80 characters at most\"}"
            Just "23505" -> Just "{\"error\": \"tag with such name already exists\"}"
            Just "0" -> Just "{\"error\": \"no such tag\"}"
            _ -> Nothing
        )

deleteTag :: Connection -> TagIdRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
deleteTag connection deleteTagRequest = let {
    params = tag_id (deleteTagRequest :: TagIdRequest);
} in do
    sessionResults <- Session.run (Session.statement params HST.deleteTag) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "23503" -> Just "{\"error\": \"tag is referenced by an article\"}"
            Just "0" -> Just "{\"error\": \"no such tag\"}"
            _ -> Nothing
        )

getTag :: Connection -> TagIdRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
getTag connection getTagRequest = let {
    params = tag_id (getTagRequest :: TagIdRequest);
} in do
    sessionResults <- Session.run (Session.statement params HST.getTag) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "0" -> Just "{\"error\": \"no such tag\"}"
            _ -> Nothing
        )


createComment :: Connection -> CreateCommentRequest -> Int32 -> IO (Either Session.QueryError ByteString, Maybe ByteString)
createComment connection createCommentRequest user_id' = let {
    params = (
        article_id (createCommentRequest :: CreateCommentRequest),
        comment_text (createCommentRequest :: CreateCommentRequest),
        user_id'
        );
} in do
    sessionResults <- Session.run (Session.statement params HST.createComment) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "23503" -> Just "{\"error\": \"no such article\"}"
            _ -> Nothing
        )

deleteComment :: Connection -> CommentIdRequest -> Int32 -> IO (Either Session.QueryError ByteString, Maybe ByteString)
deleteComment connection deleteCommentRequest user_id' = let {
    params = (
        comment_id (deleteCommentRequest :: CommentIdRequest),
        user_id'
    );
} in do
    sessionResults <- Session.run (Session.statement params HST.deleteComment) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "0" -> Just "{\"error\": \"no such comment\"}"
            _ -> Nothing
        )

getArticleComments :: Connection -> ArticleCommentsRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
getArticleComments connection articleCommentsRequest = let {
    params = (
        article_id (articleCommentsRequest :: ArticleCommentsRequest),
        offset (articleCommentsRequest :: ArticleCommentsRequest)
        );
} in do
    sessionResults <- Session.run (Session.statement params HST.getArticleComments) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )


createArticleDraft :: Connection -> ArticleDraftRequest -> Int32 -> IO (Either Session.QueryError ByteString, Maybe ByteString)
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
    sessionResults <- Session.run (Session.statement params HST.createArticleDraft) connection
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

publishArticleDraft :: Connection -> ArticleDraftIdRequest -> Int32 -> IO (Either Session.QueryError ByteString, Maybe ByteString)
publishArticleDraft connection articleDraftIdRequest author_id' = let {
    params = (
        author_id',
        article_id (articleDraftIdRequest :: ArticleDraftIdRequest)
        );
} in do
    sessionResults <- Session.run (Session.statement params HST.publishArticleDraft) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "0" -> Just "{\"error\": \"no such article\"}"
            _ -> Nothing
        )

editArticleDraft :: Connection -> ArticleDraftEditRequest -> Int32 -> IO (Either Session.QueryError ByteString, Maybe ByteString)
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
    sessionResults <- Session.run (Session.statement params HST.editArticleDraft) connection
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

getArticleDraft :: Connection -> ArticleDraftIdRequest -> Int32 -> IO (Either Session.QueryError ByteString, Maybe ByteString)
getArticleDraft connection articleDraftIdRequest author_id' = let {
    params = (
        article_id (articleDraftIdRequest :: ArticleDraftIdRequest),
        author_id'
        );
} in do
    sessionResults <- Session.run (Session.statement params HST.getArticleDraft) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "0" -> Just "{\"error\": \"no such article\"}"
            _ -> Nothing
        )

deleteArticleDraft :: Connection -> ArticleDraftIdRequest -> Int32 -> IO (Either Session.QueryError ByteString, Maybe ByteString)
deleteArticleDraft connection articleDraftIdRequest author_id' = let {
    params = (
        article_id (articleDraftIdRequest :: ArticleDraftIdRequest),
        author_id'
        );
} in do
    sessionResults <- Session.run (Session.statement params HST.deleteArticleDraft) connection
    pure (
        valueToUTFLBS sessionResults,
        case getErrorCode sessionResults of
            Just "0" -> Just "{\"error\": \"no such article\"}"
            _ -> Nothing
        )


getArticlesByCategoryId :: Connection -> ArticlesByCategoryIdRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
getArticlesByCategoryId connection articlesByCategoryIdRequest = let {
    params = (
        category_id (articlesByCategoryIdRequest :: ArticlesByCategoryIdRequest),
        offset (articlesByCategoryIdRequest :: ArticlesByCategoryIdRequest)
        );
} in do
    sessionResults <- Session.run (Session.statement params HST.getArticlesByCategoryId) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )

getArticlesByTagId :: Connection -> TagIdRequestWithOffset -> IO (Either Session.QueryError ByteString, Maybe ByteString)
getArticlesByTagId connection tagIdRequestWithOffset = let {
    params = (
        tag_id (tagIdRequestWithOffset :: TagIdRequestWithOffset),
        offset (tagIdRequestWithOffset :: TagIdRequestWithOffset)
        );
} in do
    sessionResults <- Session.run (Session.statement params HST.getArticlesByTagId) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )


getArticlesByAnyTagId :: Connection -> ArticlesByTagIdListRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
getArticlesByAnyTagId connection tagIdsRequest = let {
    params = (
        tags_ids (tagIdsRequest :: ArticlesByTagIdListRequest),
        offset (tagIdsRequest :: ArticlesByTagIdListRequest)
        );
} in do
    sessionResults <- Session.run (Session.statement params HST.getArticlesByAnyTagId) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )

getArticlesByAllTagId :: Connection -> ArticlesByTagIdListRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
getArticlesByAllTagId connection tagIdsRequest = let {
    params = (
        tags_ids (tagIdsRequest :: ArticlesByTagIdListRequest),
        offset (tagIdsRequest :: ArticlesByTagIdListRequest)
        );
} in do
    sessionResults <- Session.run (Session.statement params HST.getArticlesByAllTagId) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )

getArticlesByTitlePart :: Connection -> ArticlesByTitlePartRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
getArticlesByTitlePart connection substringRequest = let {
    params = (
        title_substring (substringRequest :: ArticlesByTitlePartRequest),
        offset (substringRequest :: ArticlesByTitlePartRequest)
        );
} in do
    sessionResults <- Session.run (Session.statement params HST.getArticlesByTitlePart) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )

getArticlesByContentPart :: Connection -> ArticlesByContentPartRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
getArticlesByContentPart connection substringRequest = let {
    params = (
        content_substring (substringRequest :: ArticlesByContentPartRequest),
        offset (substringRequest :: ArticlesByContentPartRequest)
        );
} in do
    sessionResults <- Session.run (Session.statement params HST.getArticlesByContentPart) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )

getArticlesByAuthorNamePart :: Connection -> ArticlesByAuthorNamePartRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
getArticlesByAuthorNamePart connection substringRequest = let {
    params = (
        author_name_substring (substringRequest :: ArticlesByAuthorNamePartRequest),
        offset (substringRequest :: ArticlesByAuthorNamePartRequest)
        );
} in do
    sessionResults <- Session.run (Session.statement params HST.getArticlesByAuthorNamePart) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )

getArticlesSortedByPhotosNumber :: Connection -> OffsetRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
getArticlesSortedByPhotosNumber connection request = let {
    params = (
        offset (request :: OffsetRequest)
        );
} in do
    sessionResults <- Session.run (Session.statement params HST.getArticlesSortedByPhotosNumber) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )

getArticlesSortedByCreationDate :: Connection -> OffsetRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
getArticlesSortedByCreationDate connection request = let {
    params = (
        offset (request :: OffsetRequest)
        );
} in do
    sessionResults <- Session.run (Session.statement params HST.getArticlesSortedByCreationDate) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )

getArticlesSortedByAuthor :: Connection -> OffsetRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
getArticlesSortedByAuthor connection request = let {
    params = (
        offset (request :: OffsetRequest)
        );
} in do
    sessionResults <- Session.run (Session.statement params HST.getArticlesSortedByAuthor) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )

getArticlesSortedByCategory :: Connection -> OffsetRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
getArticlesSortedByCategory connection request = let {
    params = (
        offset (request :: OffsetRequest)
        );
} in do
    sessionResults <- Session.run (Session.statement params HST.getArticlesSortedByCategory) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )

getArticlesFilteredBy :: Statement (Text, Maybe Int32) Value -> Connection -> ArticlesByCreationDateRequest
    -> IO (Either Session.QueryError ByteString, Maybe ByteString)
getArticlesFilteredBy statement connection articlesByCreationDateRequest = let {
    params = (
        pack . showGregorian $ day (articlesByCreationDateRequest :: ArticlesByCreationDateRequest),
        offset (articlesByCreationDateRequest :: ArticlesByCreationDateRequest)
        );
} in do
    sessionResults <- Session.run (Session.statement params statement) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )

getArticlesFilteredByCreationDate :: Connection -> ArticlesByCreationDateRequest
    -> IO (Either Session.QueryError ByteString, Maybe ByteString)
getArticlesFilteredByCreationDate = getArticlesFilteredBy HST.getArticlesFilteredByCreationDate

getArticlesCreatedBeforeDate :: Connection -> ArticlesByCreationDateRequest
    -> IO (Either Session.QueryError ByteString, Maybe ByteString)
getArticlesCreatedBeforeDate = getArticlesFilteredBy HST.getArticlesCreatedBeforeDate

getArticlesCreatedAfterDate :: Connection -> ArticlesByCreationDateRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
getArticlesCreatedAfterDate = getArticlesFilteredBy HST.getArticlesCreatedAfterDate


getCredentials :: Connection -> AuthRequest -> IO (Either Session.QueryError (Int32, Bool, Int32), Maybe ByteString)
getCredentials connection authRequest = let {
    params = (
        username (authRequest :: AuthRequest),
        password (authRequest :: AuthRequest)
        );
} in do
    sessionResults <- Session.run (Session.statement params HST.getCredentials) connection
    pure (
        sessionResults,
        case getErrorCode sessionResults of
            Just "0" -> Just "wrong username/password"
            _ -> Nothing
        )
