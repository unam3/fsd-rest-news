{-# LANGUAGE OverloadedStrings  #-}

module HasqlSessions (
    createUser,
    deleteUser,
    getUser
    ) where

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
    params = (name createUserRequest, surname createUserRequest, is_admin createUserRequest);
} in do
    Right connection <- Connection.acquire connectionSettings
    Session.run (Session.statement params HST.createUser) connection

deleteUser :: UserIdRequest -> IO (Either Session.QueryError ())
deleteUser deleteUserRequest = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
    params = (user_id deleteUserRequest);
} in do
    Right connection <- Connection.acquire connectionSettings
    Session.run (Session.statement params HST.deleteUser) connection

getUser :: UserIdRequest -> IO (Either Session.QueryError (Text, Text, Bool))
getUser getUserRequest = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
    params = (user_id getUserRequest);
} in do
    Right connection <- Connection.acquire connectionSettings
    Session.run (Session.statement params HST.getUser) connection
