{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module RestNews
    ( runWarpWithLogger,
    dbCall
    ) where

import Control.Exception (bracket_)
import Data.ByteString.Lazy.UTF8 (fromString)
import qualified Hasql.Connection as Connection
--import qualified Hasql.Decoders as Decoders
--import qualified Hasql.Encoders as Encoders
import Data.Text (Text)
import Hasql.Session (Session)
import Hasql.Statement (Statement(..))
import qualified Hasql.Session as Session
import qualified Hasql.TH as TH
import qualified Network.HTTP.Types as H
import Network.Wai (Application, pathInfo, responseLBS)
import Network.Wai.Handler.Warp (Port, run)
import System.Log.Logger (Priority (DEBUG, ERROR), debugM, setLevel, traplogging, updateGlobalLogger)

-- https://hackage.haskell.org/package/hasql-1.4.4
-- https://github.com/nikita-volkov/hasql-tutorial1

-- *Main RestNews> dbCall
-- Left (QueryError "INSERT INTO users VALUES (5, 'n', 's', '2010-12-12', FALSE)" [] (ResultError (UnexpectedResult "Unexpected result status: CommandOk")))
-- *Main RestNews> dbCall
-- Left (QueryError "INSERT INTO users VALUES (5, 'n', 's', '2010-12-12', FALSE)" [] (ResultError (ServerError "23505" "duplicate key value violates unique constraint \"users_pkey\"" (Just "Key (user_id)=(5) already exists.") Nothing)))

selectTest :: Session ()
selectTest = Session.statement ()
    [TH.singletonStatement|
        insert into users (name, surname, creation_date, is_admin) values
            (
                'test user name' :: Text,
                'test user surname' :: Text,
                '2010-12-12' :: date,
                false :: bool
                )
        |]

dbCall :: IO (Either Session.QueryError ())
dbCall = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
} in do
    --connectionAcquired <- Connection.acquire connectionSettings
    --(print $ case connectionAcquired of
    --    Right connection -> Session.run selectTest connection
    --    Left connectionError -> pure $ connectionError)
    Right connection <- Connection.acquire connectionSettings
    --results <- Session.run selectTest connection
    -- ? fromLeft $ Session.run selectTest connection
    Session.run selectTest connection

endpoints :: [Text]
endpoints = ["authors", "tags", "drafts", "users", "comments"]

restAPI :: Application;
restAPI request respond = let {
        pathTextChunks = pathInfo request;
        isPathEndpoint = (not (null pathTextChunks)) && elem (head pathTextChunks) endpoints;
        status = if isPathEndpoint
            then H.status200
            else H.status404;
        response = if isPathEndpoint
            then fromString $ show pathTextChunks
            else "";
    } in bracket_
        (debugM "rest-news" "Allocating scarce resource")
        (debugM "rest-news" "Cleaning up")
        --(respond $ responseLBS H.status200 [] (fromString $ show $ request))

        -- http://0.0.0.0:8081/heh/hah/wah -> ["heh","hah","wah"]
        --(respond $ responseLBS status [] (fromString $ show pathTextChunks))
        (respond $ responseLBS status [] response)

runWarp :: IO ()
runWarp = let {
    port = 8081 :: Port;
} in run port restAPI

runWarpWithLogger :: IO ()
runWarpWithLogger = runWarp
