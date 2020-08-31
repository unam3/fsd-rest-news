{-# LANGUAGE OverloadedStrings  #-}

module RestNews
    ( runWarpWithLogger
    ) where

import AesonDefinitions (CreateUserRequest)
import qualified HasqlSessions as HSS

import Control.Exception (bracket_)
import Data.Aeson (decode)
import Data.ByteString.Lazy.UTF8 (fromString)
--import Data.ByteString.Lazy (fromStrict)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Hasql.Session as Session
import qualified Network.HTTP.Types as H
-- https://hackage.haskell.org/package/http-types-0.12.3/docs/Network-HTTP-Types-Method.html#t:Method
import Network.HTTP.Types.Method (methodPost)
import Network.Wai (Application, pathInfo, requestMethod, responseLBS, strictRequestBody)
import Network.Wai.Handler.Warp (Port, run)
import System.Log.Logger (Priority (DEBUG, ERROR), debugM, setLevel, traplogging, updateGlobalLogger)

--type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
restAPI :: Application;
restAPI request respond = let {
        pathTextChunks = pathInfo request;
        -- use lists instead of list elements
        pathHeadChunk = head pathTextChunks;
        isPathHasEndpoint = (not $ null pathTextChunks) && elem pathHeadChunk endpoints;
        endpoints = ["authors", "tags", "drafts", "users", "comments"]
    } in bracket_
        (debugM "rest-news" "Allocating scarce resource")
        (debugM "rest-news" "Cleaning up")
        (do
            requestBody <- strictRequestBody request
            print requestBody

            let {
                maybeCreateUserRequestJSON = decode requestBody :: Maybe CreateUserRequest;
                status = if isPathHasEndpoint
                    then case maybeCreateUserRequestJSON of
                        Nothing -> H.status503
                        _ -> H.status200
                    else H.status404;
                IO (Left createUserError) = HSS.createUser $ fromJust maybeCreateUserRequestJSON;
                dbReturnsOk = (Session.ResultError (Session.UnexpectedResult "Unexpected result status: CommandOk"));
                response = if isPathHasEndpoint
                    then fromString $ case head pathTextChunks of
                        "authors" -> case requestMethod request of
                            methodPost -> case maybeCreateUserRequestJSON of
                                Just createUserRequest -> case createUserError of
                                    (Session.QueryError _ _ dbReturnsOk) -> "k"
                                    _ -> "<error>"
                                Nothing -> "Wrong parameters/values"
                        _ -> show pathTextChunks
                    else "No such endpoint";
            } in respond $ responseLBS status [] response)

runWarp :: IO ()
runWarp = let {
    port = 8081 :: Port;
} in run port restAPI >> pure ()

runWarpWithLogger :: IO ()
runWarpWithLogger = runWarp
