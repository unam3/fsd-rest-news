{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE OverloadedStrings  #-}

module HasqlSessions (
    createUser,
    deleteUser,
    getUser,
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
    getArticleComments
    ) where

import Data.Int (Int16)
import Data.Text (Text)
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

createUser :: CreateUserRequest -> IO (Either Session.QueryError ())
createUser createUserRequest = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
    params = (name (createUserRequest :: CreateUserRequest), surname createUserRequest, avatar createUserRequest, is_admin createUserRequest);
} in do
    Right connection <- Connection.acquire connectionSettings
    Session.run (Session.statement params HST.createUser) connection

deleteUser :: UserIdRequest -> IO (Either Session.QueryError ())
deleteUser deleteUserRequest = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
    params = user_id deleteUserRequest;
} in do
    Right connection <- Connection.acquire connectionSettings
    Session.run (Session.statement params HST.deleteUser) connection

getUser :: UserIdRequest -> IO (Either Session.QueryError (Text, Text, Text, Text, Bool))
getUser getUserRequest = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
    params = user_id getUserRequest;
} in do
    Right connection <- Connection.acquire connectionSettings
    Session.run (Session.statement params HST.getUser) connection


createCategory :: CreateCategoryRequest -> IO (Either Session.QueryError ())
createCategory createCategoryRequest = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
    params = (
        name (createCategoryRequest :: CreateCategoryRequest),
        parent_id (createCategoryRequest :: CreateCategoryRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    Session.run (Session.statement params HST.createCategory) connection

updateCategory :: UpdateCategoryRequest -> IO (Either Session.QueryError ())
updateCategory updateCategoryRequest = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
    params = (
        category_id (updateCategoryRequest :: UpdateCategoryRequest),
        name (updateCategoryRequest :: UpdateCategoryRequest),
        parent_id (updateCategoryRequest :: UpdateCategoryRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    Session.run (Session.statement params HST.updateCategory) connection

getCategory :: CategoryIdRequest -> IO (Either Session.QueryError (Text, Maybe Int16))
getCategory categoryIdRequest = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
    params = category_id (categoryIdRequest :: CategoryIdRequest);
} in do
    Right connection <- Connection.acquire connectionSettings
    Session.run (Session.statement params HST.getCategory) connection

deleteCategory :: CategoryIdRequest -> IO (Either Session.QueryError ())
deleteCategory categoryIdRequest = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
    params = category_id (categoryIdRequest :: CategoryIdRequest);
} in do
    Right connection <- Connection.acquire connectionSettings
    Session.run (Session.statement params HST.deleteCategory) connection


createTag :: CreateTagRequest -> IO (Either Session.QueryError ())
createTag createTagRequest = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
    params = tag_name (createTagRequest :: CreateTagRequest);
} in do
    Right connection <- Connection.acquire connectionSettings
    Session.run (Session.statement params HST.createTag) connection

editTag :: EditTagRequest -> IO (Either Session.QueryError ())
editTag editTagRequest = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
    params = (tag_id (editTagRequest :: EditTagRequest), tag_name (editTagRequest :: EditTagRequest));
} in do
    Right connection <- Connection.acquire connectionSettings
    Session.run (Session.statement params HST.editTag) connection

deleteTag :: TagIdRequest -> IO (Either Session.QueryError ())
deleteTag deleteTagRequest = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
    params = tag_id (deleteTagRequest :: TagIdRequest);
} in do
    Right connection <- Connection.acquire connectionSettings
    Session.run (Session.statement params HST.deleteTag) connection

getTag :: TagIdRequest -> IO (Either Session.QueryError Text)
getTag getTagRequest = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
    params = tag_id (getTagRequest :: TagIdRequest);
} in do
    --acquireResults <- Connection.acquire connectionSettings
    --case acquireResults of
    --    Left connectionError -> error $ show connectionError
    --    Right connection -> Connection.acquire connectionSettings
    Right connection <- Connection.acquire connectionSettings
    Session.run (Session.statement params HST.getTag) connection


createComment :: CreateCommentRequest -> IO (Either Session.QueryError ())
createComment createCommentRequest = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
    params = (
        news_id (createCommentRequest :: CreateCommentRequest),
        comment_text (createCommentRequest :: CreateCommentRequest)
        );
} in do
    Right connection <- Connection.acquire connectionSettings
    Session.run (Session.statement params HST.createComment) connection

deleteComment :: CommentIdRequest -> IO (Either Session.QueryError ())
deleteComment deleteCommentRequest = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
    params = comment_id (deleteCommentRequest :: CommentIdRequest);
} in do
    Right connection <- Connection.acquire connectionSettings
    Session.run (Session.statement params HST.deleteComment) connection

getArticleComments :: ArticleCommentsRequest -> IO (Either Session.QueryError (Int16, Text))
getArticleComments articleCommentsRequest = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
    params = news_id (articleCommentsRequest :: ArticleCommentsRequest);
} in do
    Right connection <- Connection.acquire connectionSettings
    Session.run (Session.statement params HST.getArticleComments) connection
