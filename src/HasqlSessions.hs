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

valueToUTFLBS :: Either Session.QueryError Value -> IO (Either Session.QueryError ByteString)
valueToUTFLBS = pure . fmap encode

connectionSettings :: Connection.Settings
connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";


createUser :: CreateUserRequest -> IO (Either Session.QueryError ByteString)
createUser createUserRequest = let {
    params = (
        username (createUserRequest :: CreateUserRequest),
        password (createUserRequest :: CreateUserRequest),
        name (createUserRequest :: CreateUserRequest),
        surname createUserRequest,
        avatar createUserRequest,
        is_admin createUserRequest
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.createUser) connection
    valueToUTFLBS sessionResults

deleteUser :: UserIdRequest -> IO (Either Session.QueryError ByteString)
deleteUser deleteUserRequest = let {
    params = user_id (deleteUserRequest :: UserIdRequest);
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.deleteUser) connection
    valueToUTFLBS sessionResults

getUser :: Int32 -> IO (Either Session.QueryError ByteString)
getUser userId = do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement userId HST.getUser) connection
    valueToUTFLBS sessionResults


promoteUserToAuthor :: PromoteUserToAuthorRequest -> IO (Either Session.QueryError ByteString)
promoteUserToAuthor promoteUserToAuthorRequest = let {
    params = (
        user_id (promoteUserToAuthorRequest :: PromoteUserToAuthorRequest),
        description (promoteUserToAuthorRequest :: PromoteUserToAuthorRequest));
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.promoteUserToAuthor) connection
    valueToUTFLBS sessionResults

editAuthor :: EditAuthorRequest -> IO (Either Session.QueryError ByteString)
editAuthor editAuthorRequest = let {
    params = (
        author_id (editAuthorRequest :: EditAuthorRequest),
        user_id (editAuthorRequest :: EditAuthorRequest),
        description (editAuthorRequest :: EditAuthorRequest));
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.editAuthor) connection
    valueToUTFLBS sessionResults

getAuthor :: AuthorIdRequest -> IO (Either Session.QueryError ByteString)
getAuthor authorIdRequest = let {
    params = author_id (authorIdRequest :: AuthorIdRequest);
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.getUser) connection
    valueToUTFLBS sessionResults

deleteAuthorRole :: AuthorIdRequest -> IO (Either Session.QueryError ByteString)
deleteAuthorRole authorIdRequest = let {
    params = author_id (authorIdRequest :: AuthorIdRequest);
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.deleteAuthorRole) connection
    valueToUTFLBS sessionResults

createCategory :: CreateCategoryRequest -> IO (Either Session.QueryError ByteString)
createCategory createCategoryRequest = let {
    params = (
        name (createCategoryRequest :: CreateCategoryRequest),
        parent_id (createCategoryRequest :: CreateCategoryRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.createCategory) connection
    valueToUTFLBS sessionResults

updateCategory :: UpdateCategoryRequest -> IO (Either Session.QueryError ByteString)
updateCategory updateCategoryRequest = let {
    params = (
        category_id (updateCategoryRequest :: UpdateCategoryRequest),
        name (updateCategoryRequest :: UpdateCategoryRequest),
        parent_id (updateCategoryRequest :: UpdateCategoryRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.updateCategory) connection
    valueToUTFLBS sessionResults

getCategory :: CategoryIdRequest -> IO (Either Session.QueryError ByteString)
getCategory categoryIdRequest = let {
    params = category_id (categoryIdRequest :: CategoryIdRequest);
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.getCategory) connection
    valueToUTFLBS sessionResults

deleteCategory :: CategoryIdRequest -> IO (Either Session.QueryError ByteString)
deleteCategory categoryIdRequest = let {
    params = category_id (categoryIdRequest :: CategoryIdRequest);
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.deleteCategory) connection
    valueToUTFLBS sessionResults


createTag :: CreateTagRequest -> IO (Either Session.QueryError ByteString)
createTag createTagRequest = let {
    params = tag_name (createTagRequest :: CreateTagRequest);
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.createTag) connection
    valueToUTFLBS sessionResults

editTag :: EditTagRequest -> IO (Either Session.QueryError ByteString)
editTag editTagRequest = let {
    params = (tag_id (editTagRequest :: EditTagRequest), tag_name (editTagRequest :: EditTagRequest));
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.editTag) connection
    valueToUTFLBS sessionResults

deleteTag :: TagIdRequest -> IO (Either Session.QueryError ByteString)
deleteTag deleteTagRequest = let {
    params = tag_id (deleteTagRequest :: TagIdRequest);
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.deleteTag) connection
    valueToUTFLBS sessionResults

getTag :: TagIdRequest -> IO (Either Session.QueryError ByteString)
getTag getTagRequest = let {
    params = tag_id (getTagRequest :: TagIdRequest);
} in do
    --acquireResults <- Connection.acquire connectionSettings
    --case acquireResults of
    --    Left connectionError -> error $ show connectionError
    --    Right connection -> Connection.acquire connectionSettings
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.getTag) connection
    valueToUTFLBS sessionResults


createComment :: CreateCommentRequest -> Int32 -> IO (Either Session.QueryError ByteString)
createComment createCommentRequest user_id' = let {
    params = (
        article_id (createCommentRequest :: CreateCommentRequest),
        comment_text (createCommentRequest :: CreateCommentRequest),
        user_id'
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.createComment) connection
    valueToUTFLBS sessionResults

deleteComment :: CommentIdRequest -> Int32 -> IO (Either Session.QueryError ByteString)
deleteComment deleteCommentRequest user_id' = let {
    params = (
        comment_id (deleteCommentRequest :: CommentIdRequest),
        user_id'
    );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.deleteComment) connection
    valueToUTFLBS sessionResults

getArticleComments :: ArticleCommentsRequest -> IO (Either Session.QueryError ByteString)
getArticleComments articleCommentsRequest = let {
    params = (
        article_id (articleCommentsRequest :: ArticleCommentsRequest),
        offset (articleCommentsRequest :: ArticleCommentsRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.getArticleComments) connection
    valueToUTFLBS sessionResults


createArticleDraft :: ArticleDraftRequest -> Int32 -> IO (Either Session.QueryError ByteString)
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
    valueToUTFLBS sessionResults

publishArticleDraft :: ArticleDraftIdRequest -> Int32 -> IO (Either Session.QueryError ByteString)
publishArticleDraft articleDraftIdRequest author_id' = let {
    params = (
        author_id',
        article_id (articleDraftIdRequest :: ArticleDraftIdRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.publishArticleDraft) connection
    valueToUTFLBS sessionResults

editArticleDraft :: ArticleDraftEditRequest -> Int32 -> IO (Either Session.QueryError ByteString)
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
    valueToUTFLBS sessionResults

getArticleDraft :: ArticleDraftIdRequest -> Int32 -> IO (Either Session.QueryError ByteString)
getArticleDraft articleDraftIdRequest author_id' = let {
    params = (
        article_id (articleDraftIdRequest :: ArticleDraftIdRequest),
        author_id'
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.getArticleDraft) connection
    valueToUTFLBS sessionResults

deleteArticleDraft :: ArticleDraftIdRequest -> Int32 -> IO (Either Session.QueryError ByteString)
deleteArticleDraft articleDraftIdRequest author_id' = let {
    params = (
        article_id (articleDraftIdRequest :: ArticleDraftIdRequest),
        author_id'
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.deleteArticleDraft) connection
    valueToUTFLBS sessionResults


getArticlesByCategoryId :: ArticlesByCategoryIdRequest -> IO (Either Session.QueryError ByteString)
getArticlesByCategoryId articlesByCategoryIdRequest = let {
    params = (
        category_id (articlesByCategoryIdRequest :: ArticlesByCategoryIdRequest),
        offset (articlesByCategoryIdRequest :: ArticlesByCategoryIdRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.getArticlesByCategoryId) connection
    valueToUTFLBS sessionResults

getArticlesByTagId :: TagIdRequestWithOffset -> IO (Either Session.QueryError ByteString)
getArticlesByTagId tagIdRequestWithOffset = let {
    params = (
        tag_id (tagIdRequestWithOffset :: TagIdRequestWithOffset),
        offset (tagIdRequestWithOffset :: TagIdRequestWithOffset)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.getArticlesByTagId) connection
    valueToUTFLBS sessionResults


getArticlesByAnyTagId :: ArticlesByTagIdListRequest -> IO (Either Session.QueryError ByteString)
getArticlesByAnyTagId tagIdsRequest = let {
    params = (
        tags_ids (tagIdsRequest :: ArticlesByTagIdListRequest),
        offset (tagIdsRequest :: ArticlesByTagIdListRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.getArticlesByAnyTagId) connection
    valueToUTFLBS sessionResults

getArticlesByAllTagId :: ArticlesByTagIdListRequest -> IO (Either Session.QueryError ByteString)
getArticlesByAllTagId tagIdsRequest = let {
    params = (
        tags_ids (tagIdsRequest :: ArticlesByTagIdListRequest),
        offset (tagIdsRequest :: ArticlesByTagIdListRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.getArticlesByAllTagId) connection
    valueToUTFLBS sessionResults

getArticlesByTitlePart :: ArticlesByTitlePartRequest -> IO (Either Session.QueryError ByteString)
getArticlesByTitlePart substringRequest = let {
    params = (
        title_substring (substringRequest :: ArticlesByTitlePartRequest),
        offset (substringRequest :: ArticlesByTitlePartRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.getArticlesByTitlePart) connection
    --Data.Text.IO.putStrLn . pack $ show sessionResults
    valueToUTFLBS sessionResults

getArticlesByContentPart :: ArticlesByContentPartRequest -> IO (Either Session.QueryError ByteString)
getArticlesByContentPart substringRequest = let {
    params = (
        content_substring (substringRequest :: ArticlesByContentPartRequest),
        offset (substringRequest :: ArticlesByContentPartRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.getArticlesByContentPart) connection
    valueToUTFLBS sessionResults

getArticlesByAuthorNamePart :: ArticlesByAuthorNamePartRequest -> IO (Either Session.QueryError ByteString)
getArticlesByAuthorNamePart substringRequest = let {
    params = (
        author_name_substring (substringRequest :: ArticlesByAuthorNamePartRequest),
        offset (substringRequest :: ArticlesByAuthorNamePartRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.getArticlesByAuthorNamePart) connection
    valueToUTFLBS sessionResults

getArticlesSortedByPhotosNumber :: OffsetRequest -> IO (Either Session.QueryError ByteString)
getArticlesSortedByPhotosNumber request = let {
    params = (
        offset (request :: OffsetRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.getArticlesSortedByPhotosNumber) connection
    valueToUTFLBS sessionResults

getArticlesSortedByCreationDate :: OffsetRequest -> IO (Either Session.QueryError ByteString)
getArticlesSortedByCreationDate request = let {
    params = (
        offset (request :: OffsetRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.getArticlesSortedByCreationDate) connection
    valueToUTFLBS sessionResults

getArticlesSortedByAuthor :: OffsetRequest -> IO (Either Session.QueryError ByteString)
getArticlesSortedByAuthor request = let {
    params = (
        offset (request :: OffsetRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.getArticlesSortedByAuthor) connection
    valueToUTFLBS sessionResults

getArticlesSortedByCategory :: OffsetRequest -> IO (Either Session.QueryError ByteString)
getArticlesSortedByCategory request = let {
    params = (
        offset (request :: OffsetRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.getArticlesSortedByCategory) connection
    valueToUTFLBS sessionResults

getArticlesFilteredBy :: Statement Text Value -> ArticlesByCreationDateRequest
    -> IO (Either Session.QueryError ByteString)
getArticlesFilteredBy filterF articlesByCreationDateRequest = let {
    params = (
        pack . showGregorian $ day (articlesByCreationDateRequest :: ArticlesByCreationDateRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params filterF) connection
    valueToUTFLBS sessionResults

getArticlesFilteredByCreationDate :: ArticlesByCreationDateRequest -> IO (Either Session.QueryError ByteString)
getArticlesFilteredByCreationDate = getArticlesFilteredBy HST.getArticlesFilteredByCreationDate

getArticlesCreatedBeforeDate :: ArticlesByCreationDateRequest -> IO (Either Session.QueryError ByteString)
getArticlesCreatedBeforeDate = getArticlesFilteredBy HST.getArticlesCreatedBeforeDate

getArticlesCreatedAfterDate :: ArticlesByCreationDateRequest -> IO (Either Session.QueryError ByteString)
getArticlesCreatedAfterDate = getArticlesFilteredBy HST.getArticlesCreatedAfterDate


getCredentials :: AuthRequest -> IO (Either Session.QueryError (Int32, Bool, Int32))
getCredentials authRequest = let {
    params = (
        username (authRequest :: AuthRequest),
        password (authRequest :: AuthRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.getCredentials) connection
    pure sessionResults


