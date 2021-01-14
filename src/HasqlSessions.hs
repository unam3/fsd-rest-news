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

import Data.Aeson (Value, encode)
import Data.ByteString.Lazy.UTF8 (ByteString)
import Data.Int (Int32)
import Data.Text (Text, pack)
--import Data.Text.IO (putStrLn)
import Data.Time.Calendar (showGregorian)
import qualified Hasql.Connection as Connection
import qualified Hasql.Session as Session
import Hasql.Statement (Statement)

import AesonDefinitions
import qualified HasqlStatements as HST

-- https://hackage.haskell.org/package/hasql-1.4.4
-- https://github.com/nikita-volkov/hasql-tutorial1

-- *Main RestNews> dbCall
-- Left (QueryError "INSERT INTO users VALUES (5, 'n', 's', '2010-12-12', FALSE)" [] (ResultError (UnexpectedResult "Unexpected result status: CommandOk")))
-- *Main RestNews> dbCall
-- Left (QueryError "INSERT INTO users VALUES (5, 'n', 's', '2010-12-12', FALSE)" [] (ResultError (ServerError "23505" "duplicate key value violates unique constraint \"users_pkey\"" (Just "Key (user_id)=(5) already exists.") Nothing)))

--int32ToUTFLBS $ Session.run (Session.statement params HST.createUser) connection
--int32ToUTFLBS = fmap $ fmap (UTFLBS.fromString . show)

-- Text
--(fmap (fmap (UTFLBS.fromString . unpack))) $ Session.run (Session.statement params HST.createUser) connection

valueToUTFLBS :: Either Session.QueryError Value -> Either Session.QueryError ByteString
valueToUTFLBS = fmap encode

connectionSettings :: Connection.Settings
connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";

processError :: Either Session.QueryError resultsType -> Maybe String
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
processError (Left (Session.QueryError _ _ (Session.ResultError (Session.ServerError "23503" _ _ _)))) = Just "23503"
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
processError (Left (Session.QueryError _ _ (Session.ResultError (Session.ServerError "23505" _ _ _)))) = Just "23505"
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
processError (Left (Session.QueryError _ _ (Session.ResultError (Session.UnexpectedAmountOfRows 0)))) = Just "0"
-- (Nothing,Left (QueryError "SELECT json_agg(users.*) :: json FROM users WHERE user_id = $1 :: int4" ["20"] (ResultError (RowError 0 UnexpectedNull))))
processError (Left (Session.QueryError _ _ (Session.ResultError (Session.RowError 0 Session.UnexpectedNull)))) = Just "0"
processError _ = Nothing

-- params and statement type are different
--runSession params statement =
--    Right connection <- Connection.acquire connectionSettings
--    sessionResults <- Session.run (Session.statement params statement) connection

--getConnection :: IO (Either Connection.ConnectionError Connection.Connection)
--getConnection = --Connection.acquire connectionSettings
--    do
--    eitherConnection <- Connection.acquire connectionSettings
--    pure $ case eitherConnection of
--        Right connection -> connection
--        Left connectionError -> error $ show connectionError
--        --Left Connection.ConnectionError -> undefined

createUser :: CreateUserRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
createUser createUserRequest = let {
    params = (
        username (createUserRequest :: CreateUserRequest),
        password (createUserRequest :: CreateUserRequest),
        name (createUserRequest :: CreateUserRequest),
        surname createUserRequest,
        avatar createUserRequest
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.createUser) connection
    pure (
        valueToUTFLBS sessionResults,
        case processError sessionResults of
            Just "23505" -> Just "{\"error\": \"user with this username already exists\"}"
            _ -> Nothing
        )

deleteUser :: UserIdRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
deleteUser deleteUserRequest = let {
    params = user_id (deleteUserRequest :: UserIdRequest);
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.deleteUser) connection
    pure (
        valueToUTFLBS sessionResults,
        case processError sessionResults of
            Just "0" -> Just "{\"error\": \"such user does not exist\"}"
            _ -> Nothing
        )

getUser :: Int32 -> IO (Either Session.QueryError ByteString, Maybe ByteString)
getUser userId = do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement userId HST.getUser) connection
    pure (
        valueToUTFLBS sessionResults,
        case processError sessionResults of
            Just "0" -> Just "{\"error\": \"such user does not exist\"}"
            _ -> Nothing
        )


promoteUserToAuthor :: PromoteUserToAuthorRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
promoteUserToAuthor promoteUserToAuthorRequest = let {
    params = (
        user_id (promoteUserToAuthorRequest :: PromoteUserToAuthorRequest),
        description (promoteUserToAuthorRequest :: PromoteUserToAuthorRequest));
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.promoteUserToAuthor) connection
    pure (
        valueToUTFLBS sessionResults,
        case processError sessionResults of
            Just "23503" -> Just "{\"error\": \"such user does not exist\"}"
            Just "23505" -> Just "{\"error\": \"such user is already an author\"}"
            _ -> Nothing
        )

editAuthor :: EditAuthorRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
editAuthor editAuthorRequest = let {
    params = (
        author_id (editAuthorRequest :: EditAuthorRequest),
        description (editAuthorRequest :: EditAuthorRequest));
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.editAuthor) connection
    pure (
        valueToUTFLBS sessionResults,
        case processError sessionResults of
            Just "0" -> Just "{\"error\": \"such author does not exist\"}"
            _ -> Nothing
        )

getAuthor :: AuthorIdRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
getAuthor authorIdRequest = let {
    params = author_id (authorIdRequest :: AuthorIdRequest);
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.getAuthor) connection
    pure (
        valueToUTFLBS sessionResults,
        case processError sessionResults of
            Just "0" -> Just "{\"error\": \"such author does not exist\"}"
            _ -> Nothing
        )

deleteAuthorRole :: AuthorIdRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
deleteAuthorRole authorIdRequest = let {
    params = author_id (authorIdRequest :: AuthorIdRequest);
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.deleteAuthorRole) connection
    pure (
        valueToUTFLBS sessionResults,
        case processError sessionResults of
            Just "0" -> Just "{\"error\": \"such author does not exist\"}"
            _ -> Nothing
        )

createCategory :: CreateCategoryRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
createCategory createCategoryRequest = let {
    params = (
        name (createCategoryRequest :: CreateCategoryRequest),
        parent_id (createCategoryRequest :: CreateCategoryRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.createCategory) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )

updateCategory :: UpdateCategoryRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
updateCategory updateCategoryRequest = let {
    params = (
        category_id (updateCategoryRequest :: UpdateCategoryRequest),
        name (updateCategoryRequest :: UpdateCategoryRequest),
        parent_id (updateCategoryRequest :: UpdateCategoryRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.updateCategory) connection
    pure (
        valueToUTFLBS sessionResults,
        case processError sessionResults of
            Just "0" -> Just "{\"error\": \"no such category\"}"
            _ -> Nothing
        )

getCategory :: CategoryIdRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
getCategory categoryIdRequest = let {
    params = category_id (categoryIdRequest :: CategoryIdRequest);
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.getCategory) connection
    pure (
        valueToUTFLBS sessionResults,
        case processError sessionResults of
            Just "0" -> Just "{\"error\": \"no such category\"}"
            _ -> Nothing
        )

deleteCategory :: CategoryIdRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
deleteCategory categoryIdRequest = let {
    params = category_id (categoryIdRequest :: CategoryIdRequest);
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.deleteCategory) connection
    pure (
        valueToUTFLBS sessionResults,
        case processError sessionResults of
            Just "23503" -> Just "{\"error\": \"category is in use\"}"
            Just "0" -> Just "{\"error\": \"no such category\"}"
            _ -> Nothing
        )


createTag :: CreateTagRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
createTag createTagRequest = let {
    params = tag_name (createTagRequest :: CreateTagRequest);
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.createTag) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )

editTag :: EditTagRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
editTag editTagRequest = let {
    params = (tag_id (editTagRequest :: EditTagRequest), tag_name (editTagRequest :: EditTagRequest));
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.editTag) connection
    pure (
        valueToUTFLBS sessionResults,
        case processError sessionResults of
            Just "0" -> Just "{\"error\": \"no such tag\"}"
            _ -> Nothing
        )

deleteTag :: TagIdRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
deleteTag deleteTagRequest = let {
    params = tag_id (deleteTagRequest :: TagIdRequest);
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.deleteTag) connection
    pure (
        valueToUTFLBS sessionResults,
        case processError sessionResults of
            Just "23503" -> Just "{\"error\": \"tag is in use\"}"
            Just "0" -> Just "{\"error\": \"no such tag\"}"
            _ -> Nothing
        )

getTag :: TagIdRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
getTag getTagRequest = let {
    params = tag_id (getTagRequest :: TagIdRequest);
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.getTag) connection
    pure (
        valueToUTFLBS sessionResults,
        case processError sessionResults of
            Just "0" -> Just "{\"error\": \"no such tag\"}"
            _ -> Nothing
        )


createComment :: CreateCommentRequest -> Int32 -> IO (Either Session.QueryError ByteString, Maybe ByteString)
createComment createCommentRequest user_id' = let {
    params = (
        article_id (createCommentRequest :: CreateCommentRequest),
        comment_text (createCommentRequest :: CreateCommentRequest),
        user_id'
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.createComment) connection
    pure (
        valueToUTFLBS sessionResults,
        case processError sessionResults of
            Just "23503" -> Just "{\"error\": \"no such article\"}"
            _ -> Nothing
        )

deleteComment :: CommentIdRequest -> Int32 -> IO (Either Session.QueryError ByteString, Maybe ByteString)
deleteComment deleteCommentRequest user_id' = let {
    params = (
        comment_id (deleteCommentRequest :: CommentIdRequest),
        user_id'
    );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.deleteComment) connection
    pure (
        valueToUTFLBS sessionResults,
        case processError sessionResults of
            Just "0" -> Just "{\"error\": \"no such comment\"}"
            _ -> Nothing
        )

getArticleComments :: ArticleCommentsRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
getArticleComments articleCommentsRequest = let {
    params = (
        article_id (articleCommentsRequest :: ArticleCommentsRequest),
        offset (articleCommentsRequest :: ArticleCommentsRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.getArticleComments) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )


createArticleDraft :: ArticleDraftRequest -> Int32 -> IO (Either Session.QueryError ByteString, Maybe ByteString)
createArticleDraft articleDraftRequest author_id' = let {
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
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.createArticleDraft) connection
    pure (
        valueToUTFLBS sessionResults,
        Nothing
        )

publishArticleDraft :: ArticleDraftIdRequest -> Int32 -> IO (Either Session.QueryError ByteString, Maybe ByteString)
publishArticleDraft articleDraftIdRequest author_id' = let {
    params = (
        author_id',
        article_id (articleDraftIdRequest :: ArticleDraftIdRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.publishArticleDraft) connection
    pure (
        valueToUTFLBS sessionResults,
        case processError sessionResults of
            --Just "23505" -> Just "user with this username already exists"
            _ -> Nothing
        )

editArticleDraft :: ArticleDraftEditRequest -> Int32 -> IO (Either Session.QueryError ByteString, Maybe ByteString)
editArticleDraft articleDraftEditRequest author_id' = let {
    params = (
        author_id',
        article_id (articleDraftEditRequest :: ArticleDraftEditRequest),
        category_id (articleDraftEditRequest :: ArticleDraftEditRequest),
        article_title (articleDraftEditRequest :: ArticleDraftEditRequest),
        article_content (articleDraftEditRequest :: ArticleDraftEditRequest),
        main_photo (articleDraftEditRequest :: ArticleDraftEditRequest),
        additional_photos (articleDraftEditRequest :: ArticleDraftEditRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.editArticleDraft) connection
    pure (
        valueToUTFLBS sessionResults,
        case processError sessionResults of
            Just "23503" -> Just "{\"error\": \"no such category\"}"
            Just "0" -> Just "{\"error\": \"no such article\"}"
            _ -> Nothing
        )

getArticleDraft :: ArticleDraftIdRequest -> Int32 -> IO (Either Session.QueryError ByteString, Maybe ByteString)
getArticleDraft articleDraftIdRequest author_id' = let {
    params = (
        article_id (articleDraftIdRequest :: ArticleDraftIdRequest),
        author_id'
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.getArticleDraft) connection
    pure (
        valueToUTFLBS sessionResults,
        case processError sessionResults of
            --Just "23505" -> Just "user with this username already exists"
            _ -> Nothing
        )

deleteArticleDraft :: ArticleDraftIdRequest -> Int32 -> IO (Either Session.QueryError ByteString, Maybe ByteString)
deleteArticleDraft articleDraftIdRequest author_id' = let {
    params = (
        article_id (articleDraftIdRequest :: ArticleDraftIdRequest),
        author_id'
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.deleteArticleDraft) connection
    pure (
        valueToUTFLBS sessionResults,
        case processError sessionResults of
            --Just "23505" -> Just "user with this username already exists"
            _ -> Nothing
        )


getArticlesByCategoryId :: ArticlesByCategoryIdRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
getArticlesByCategoryId articlesByCategoryIdRequest = let {
    params = (
        category_id (articlesByCategoryIdRequest :: ArticlesByCategoryIdRequest),
        offset (articlesByCategoryIdRequest :: ArticlesByCategoryIdRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.getArticlesByCategoryId) connection
    pure (
        valueToUTFLBS sessionResults,
        case processError sessionResults of
            --Just "23505" -> Just "user with this username already exists"
            _ -> Nothing
        )

getArticlesByTagId :: TagIdRequestWithOffset -> IO (Either Session.QueryError ByteString, Maybe ByteString)
getArticlesByTagId tagIdRequestWithOffset = let {
    params = (
        tag_id (tagIdRequestWithOffset :: TagIdRequestWithOffset),
        offset (tagIdRequestWithOffset :: TagIdRequestWithOffset)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.getArticlesByTagId) connection
    pure (
        valueToUTFLBS sessionResults,
        case processError sessionResults of
            --Just "23505" -> Just "user with this username already exists"
            _ -> Nothing
        )


getArticlesByAnyTagId :: ArticlesByTagIdListRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
getArticlesByAnyTagId tagIdsRequest = let {
    params = (
        tags_ids (tagIdsRequest :: ArticlesByTagIdListRequest),
        offset (tagIdsRequest :: ArticlesByTagIdListRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.getArticlesByAnyTagId) connection
    pure (
        valueToUTFLBS sessionResults,
        case processError sessionResults of
            --Just "23505" -> Just "user with this username already exists"
            _ -> Nothing
        )

getArticlesByAllTagId :: ArticlesByTagIdListRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
getArticlesByAllTagId tagIdsRequest = let {
    params = (
        tags_ids (tagIdsRequest :: ArticlesByTagIdListRequest),
        offset (tagIdsRequest :: ArticlesByTagIdListRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.getArticlesByAllTagId) connection
    pure (
        valueToUTFLBS sessionResults,
        case processError sessionResults of
            --Just "23505" -> Just "user with this username already exists"
            _ -> Nothing
        )

getArticlesByTitlePart :: ArticlesByTitlePartRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
getArticlesByTitlePart substringRequest = let {
    params = (
        title_substring (substringRequest :: ArticlesByTitlePartRequest),
        offset (substringRequest :: ArticlesByTitlePartRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.getArticlesByTitlePart) connection
    --Data.Text.IO.putStrLn . pack $ show sessionResults
    pure (
        valueToUTFLBS sessionResults,
        case processError sessionResults of
            --Just "23505" -> Just "user with this username already exists"
            _ -> Nothing
        )

getArticlesByContentPart :: ArticlesByContentPartRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
getArticlesByContentPart substringRequest = let {
    params = (
        content_substring (substringRequest :: ArticlesByContentPartRequest),
        offset (substringRequest :: ArticlesByContentPartRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.getArticlesByContentPart) connection
    pure (
        valueToUTFLBS sessionResults,
        case processError sessionResults of
            --Just "23505" -> Just "user with this username already exists"
            _ -> Nothing
        )

getArticlesByAuthorNamePart :: ArticlesByAuthorNamePartRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
getArticlesByAuthorNamePart substringRequest = let {
    params = (
        author_name_substring (substringRequest :: ArticlesByAuthorNamePartRequest),
        offset (substringRequest :: ArticlesByAuthorNamePartRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.getArticlesByAuthorNamePart) connection
    pure (
        valueToUTFLBS sessionResults,
        case processError sessionResults of
            --Just "23505" -> Just "user with this username already exists"
            _ -> Nothing
        )

getArticlesSortedByPhotosNumber :: OffsetRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
getArticlesSortedByPhotosNumber request = let {
    params = (
        offset (request :: OffsetRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.getArticlesSortedByPhotosNumber) connection
    pure (
        valueToUTFLBS sessionResults,
        case processError sessionResults of
            --Just "23505" -> Just "user with this username already exists"
            _ -> Nothing
        )

getArticlesSortedByCreationDate :: OffsetRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
getArticlesSortedByCreationDate request = let {
    params = (
        offset (request :: OffsetRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.getArticlesSortedByCreationDate) connection
    pure (
        valueToUTFLBS sessionResults,
        case processError sessionResults of
            --Just "23505" -> Just "user with this username already exists"
            _ -> Nothing
        )

getArticlesSortedByAuthor :: OffsetRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
getArticlesSortedByAuthor request = let {
    params = (
        offset (request :: OffsetRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.getArticlesSortedByAuthor) connection
    pure (
        valueToUTFLBS sessionResults,
        case processError sessionResults of
            --Just "23505" -> Just "user with this username already exists"
            _ -> Nothing
        )

getArticlesSortedByCategory :: OffsetRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
getArticlesSortedByCategory request = let {
    params = (
        offset (request :: OffsetRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.getArticlesSortedByCategory) connection
    pure (
        valueToUTFLBS sessionResults,
        case processError sessionResults of
            --Just "23505" -> Just "user with this username already exists"
            _ -> Nothing
        )

getArticlesFilteredBy :: Statement (Text, Maybe Int32) Value -> ArticlesByCreationDateRequest
    -> IO (Either Session.QueryError ByteString, Maybe ByteString)
getArticlesFilteredBy filterF articlesByCreationDateRequest = let {
    params = (
        pack . showGregorian $ day (articlesByCreationDateRequest :: ArticlesByCreationDateRequest),
        offset (articlesByCreationDateRequest :: ArticlesByCreationDateRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params filterF) connection
    pure (
        valueToUTFLBS sessionResults,
        case processError sessionResults of
            --Just "23505" -> Just "user with this username already exists"
            _ -> Nothing
        )

getArticlesFilteredByCreationDate :: ArticlesByCreationDateRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
getArticlesFilteredByCreationDate = getArticlesFilteredBy HST.getArticlesFilteredByCreationDate

getArticlesCreatedBeforeDate :: ArticlesByCreationDateRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
getArticlesCreatedBeforeDate = getArticlesFilteredBy HST.getArticlesCreatedBeforeDate

getArticlesCreatedAfterDate :: ArticlesByCreationDateRequest -> IO (Either Session.QueryError ByteString, Maybe ByteString)
getArticlesCreatedAfterDate = getArticlesFilteredBy HST.getArticlesCreatedAfterDate


getCredentials :: AuthRequest -> IO (Either Session.QueryError (Int32, Bool, Int32), Maybe ByteString)
getCredentials authRequest = let {
    params = (
        username (authRequest :: AuthRequest),
        password (authRequest :: AuthRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.getCredentials) connection
    pure (
        sessionResults,
        case processError sessionResults of
            Just "0" -> Just "wrong username/password"
            _ -> Nothing
        )
