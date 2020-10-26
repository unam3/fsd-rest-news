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
    getArticleDraft,
    getArticlesByCategoryId,
    getArticlesByTagId,
    getArticlesByAnyTagId,
    getArticlesByAllTagId,
    getArticlesByTitlePart,
    getArticlesByContentPart,
    getArticlesByAuthorNamePart
    ) where

import Data.Aeson (Value, encode)
import Data.ByteString.Lazy.UTF8 (ByteString)
--import Data.Text (Text, unpack, pack)
--import Data.Text.IO (putStrLn)
import qualified Hasql.Connection as Connection
import qualified Hasql.Session as Session

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

createUser :: CreateUserRequest -> IO (Either Session.QueryError ByteString)
createUser createUserRequest = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
    params = (name (createUserRequest :: CreateUserRequest), surname createUserRequest, avatar createUserRequest, is_admin createUserRequest);
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.createUser) connection
    pure (fmap encode sessionResults)

deleteUser :: UserIdRequest -> IO (Either Session.QueryError ())
deleteUser deleteUserRequest = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
    params = user_id (deleteUserRequest :: UserIdRequest);
} in do
    Right connection <- Connection.acquire connectionSettings
    Session.run (Session.statement params HST.deleteUser) connection

getUser :: UserIdRequest -> IO (Either Session.QueryError ByteString)
getUser getUserRequest = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
    params = user_id (getUserRequest :: UserIdRequest);
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.getUser) connection
    pure (fmap encode sessionResults)


promoteUserToAuthor :: PromoteUserToAuthorRequest -> IO (Either Session.QueryError ByteString)
promoteUserToAuthor promoteUserToAuthorRequest = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
    params = (
        user_id (promoteUserToAuthorRequest :: PromoteUserToAuthorRequest),
        description (promoteUserToAuthorRequest :: PromoteUserToAuthorRequest));
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.promoteUserToAuthor) connection
    valueToUTFLBS sessionResults

editAuthor :: EditAuthorRequest -> IO (Either Session.QueryError ByteString)
editAuthor editAuthorRequest = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
    params = (
        author_id (editAuthorRequest :: EditAuthorRequest),
        user_id (editAuthorRequest :: EditAuthorRequest),
        description (editAuthorRequest :: EditAuthorRequest));
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.editAuthor) connection
    pure (fmap encode sessionResults)

getAuthor :: AuthorIdRequest -> IO (Either Session.QueryError ByteString)
getAuthor authorIdRequest = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
    params = author_id (authorIdRequest :: AuthorIdRequest);
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.getUser) connection
    valueToUTFLBS sessionResults

deleteAuthorRole :: AuthorIdRequest -> IO (Either Session.QueryError ())
deleteAuthorRole authorIdRequest = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
    params = author_id (authorIdRequest :: AuthorIdRequest);
} in do
    Right connection <- Connection.acquire connectionSettings
    Session.run (Session.statement params HST.deleteAuthorRole) connection

createCategory :: CreateCategoryRequest -> IO (Either Session.QueryError ByteString)
createCategory createCategoryRequest = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
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
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
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
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
    params = category_id (categoryIdRequest :: CategoryIdRequest);
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.getCategory) connection
    valueToUTFLBS sessionResults

deleteCategory :: CategoryIdRequest -> IO (Either Session.QueryError ())
deleteCategory categoryIdRequest = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
    params = category_id (categoryIdRequest :: CategoryIdRequest);
} in do
    Right connection <- Connection.acquire connectionSettings
    Session.run (Session.statement params HST.deleteCategory) connection


createTag :: CreateTagRequest -> IO (Either Session.QueryError ByteString)
createTag createTagRequest = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
    params = tag_name (createTagRequest :: CreateTagRequest);
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.createTag) connection
    valueToUTFLBS sessionResults

editTag :: EditTagRequest -> IO (Either Session.QueryError ByteString)
editTag editTagRequest = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
    params = (tag_id (editTagRequest :: EditTagRequest), tag_name (editTagRequest :: EditTagRequest));
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.editTag) connection
    valueToUTFLBS sessionResults

deleteTag :: TagIdRequest -> IO (Either Session.QueryError ())
deleteTag deleteTagRequest = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
    params = tag_id (deleteTagRequest :: TagIdRequest);
} in do
    Right connection <- Connection.acquire connectionSettings
    Session.run (Session.statement params HST.deleteTag) connection

getTag :: TagIdRequest -> IO (Either Session.QueryError ByteString)
getTag getTagRequest = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
    params = tag_id (getTagRequest :: TagIdRequest);
} in do
    --acquireResults <- Connection.acquire connectionSettings
    --case acquireResults of
    --    Left connectionError -> error $ show connectionError
    --    Right connection -> Connection.acquire connectionSettings
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.getTag) connection
    valueToUTFLBS sessionResults


createComment :: CreateCommentRequest -> IO (Either Session.QueryError ByteString)
createComment createCommentRequest = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
    params = (
        article_id (createCommentRequest :: CreateCommentRequest),
        comment_text (createCommentRequest :: CreateCommentRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.createComment) connection
    valueToUTFLBS sessionResults

deleteComment :: CommentIdRequest -> IO (Either Session.QueryError ())
deleteComment deleteCommentRequest = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
    params = comment_id (deleteCommentRequest :: CommentIdRequest);
} in do
    Right connection <- Connection.acquire connectionSettings
    Session.run (Session.statement params HST.deleteComment) connection

getArticleComments :: ArticleCommentsRequest -> IO (Either Session.QueryError ByteString)
getArticleComments articleCommentsRequest = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
    params = article_id (articleCommentsRequest :: ArticleCommentsRequest);
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.getArticleComments) connection
    valueToUTFLBS sessionResults


createArticleDraft :: ArticleDraftRequest -> IO (Either Session.QueryError ByteString)
createArticleDraft articleDraftRequest = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
    params = (
        author (articleDraftRequest :: ArticleDraftRequest),
        category_id (articleDraftRequest :: ArticleDraftRequest),
        article_title (articleDraftRequest :: ArticleDraftRequest),
        article_content (articleDraftRequest :: ArticleDraftRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.createArticleDraft) connection
    valueToUTFLBS sessionResults

publishArticleDraft :: ArticleDraftIdRequest -> IO (Either Session.QueryError ByteString)
publishArticleDraft articleDraftIdRequest = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
    params = (
        article_id (articleDraftIdRequest :: ArticleDraftIdRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- Session.run (Session.statement params HST.publishArticleDraft) connection
    valueToUTFLBS sessionResults

getArticleDraft :: ArticleDraftIdRequest -> IO (Either Session.QueryError ByteString)
getArticleDraft articleDraftIdRequest = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
    params = (
        article_id (articleDraftIdRequest :: ArticleDraftIdRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- (Session.run (Session.statement params HST.getArticleDraft) connection)
    valueToUTFLBS sessionResults


getArticlesByCategoryId :: ArticlesByCategoryIdRequest -> IO (Either Session.QueryError ByteString)
getArticlesByCategoryId articlesByCategoryIdRequest = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
    params = (
        category_id (articlesByCategoryIdRequest :: ArticlesByCategoryIdRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- (Session.run (Session.statement params HST.getArticlesByCategoryId) connection)
    pure (fmap encode sessionResults)

getArticlesByTagId :: TagIdRequest -> IO (Either Session.QueryError ByteString)
getArticlesByTagId tagIdRequest = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
    params = (
        tag_id (tagIdRequest :: TagIdRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- (Session.run (Session.statement params HST.getArticlesByTagId) connection)
    pure (fmap encode sessionResults)


getArticlesByAnyTagId :: ArticlesByTagIdListRequest -> IO (Either Session.QueryError ByteString)
getArticlesByAnyTagId tagIdsRequest = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
    params = (
        tags_ids (tagIdsRequest :: ArticlesByTagIdListRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- (Session.run (Session.statement params HST.getArticlesByAnyTagId) connection)
    pure (fmap encode sessionResults)

getArticlesByAllTagId :: ArticlesByTagIdListRequest -> IO (Either Session.QueryError ByteString)
getArticlesByAllTagId tagIdsRequest = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
    params = (
        tags_ids (tagIdsRequest :: ArticlesByTagIdListRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- (Session.run (Session.statement params HST.getArticlesByAllTagId) connection)
    pure (fmap encode sessionResults)

getArticlesByTitlePart :: ArticlesByTitlePartRequest -> IO (Either Session.QueryError ByteString)
getArticlesByTitlePart substringRequest = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
    params = (
        title_substring (substringRequest :: ArticlesByTitlePartRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- (Session.run (Session.statement params HST.getArticlesByTitlePart) connection)
    --Data.Text.IO.putStrLn . pack $ show sessionResults
    pure (fmap encode sessionResults)

getArticlesByContentPart :: ArticlesByContentPartRequest -> IO (Either Session.QueryError ByteString)
getArticlesByContentPart substringRequest = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
    params = (
        content_substring (substringRequest :: ArticlesByContentPartRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- (Session.run (Session.statement params HST.getArticlesByContentPart) connection)
    pure (fmap encode sessionResults)

getArticlesByAuthorNamePart :: ArticlesByAuthorNamePartRequest -> IO (Either Session.QueryError ByteString)
getArticlesByAuthorNamePart substringRequest = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
    params = (
        author_name_substring (substringRequest :: ArticlesByAuthorNamePartRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    sessionResults <- (Session.run (Session.statement params HST.getArticlesByAuthorNamePart) connection)
    pure (fmap encode sessionResults)
    --pure sessionResults
