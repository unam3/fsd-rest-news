{-# LANGUAGE OverloadedStrings  #-}

module RestNews
    ( runWarpWithLogger
    ) where

import AesonDefinitions (CreateUserRequest, UserIdRequest)
import qualified HasqlSessions as HSS

import Control.Exception (bracket_)
import Data.Aeson (decode)
--import Data.ByteString.Lazy.UTF8 (fromString)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (fromStrict)
import Data.Maybe (fromJust)
import qualified Hasql.Session as Session
import qualified Network.HTTP.Types as H
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
        endpoints = ["authors", "tags", "drafts", "users", "comments"];
    } in bracket_
        (debugM "rest-news" "Allocating scarce resource")
        (debugM "rest-news" "Cleaning up")
        (do
            requestBody <- strictRequestBody request
            print requestBody

            errorOrSessionName <- let {
                maybeCreateUserRequestJSON = decode requestBody :: Maybe CreateUserRequest;
                maybeUserIdRequestJSON = decode requestBody :: Maybe UserIdRequest;
            } in pure (
                if isPathHasEndpoint
                    then (case pathHeadChunk of
                        "authors" -> case requestMethod request of
                            "POST" -> case maybeCreateUserRequestJSON of
                                Just _ -> "createUser"
                                Nothing -> "Wrong parameters/parameters values"
                            "GET" -> case maybeUserIdRequestJSON of
                                Just _ -> "getUser"
                                Nothing -> "Wrong parameters/parameters values"
                            "DELETE" -> case maybeUserIdRequestJSON of
                                Just _ -> "deleteUser"
                                Nothing -> "Wrong parameters/parameters values"
                            _ -> "Method is not implemented"
                        _ -> show pathTextChunks)
                    else "No such endpoint")

            results <- case errorOrSessionName of
                "createUser" -> do
                    sessionResults <- HSS.createUser $ fromJust (decode requestBody :: Maybe CreateUserRequest)
                    pure $ case sessionResults of
                        Left (Session.QueryError _ _ (Session.ResultError (Session.UnexpectedResult "Unexpected result status: CommandOk"))) -> "ok"
                        Left (Session.QueryError _ _ (Session.ResultError sessionError)) -> fromStrict . pack $ show sessionError
                        _ -> "eeh?"
                "getUser" -> do
                    sessionResults <- HSS.getUser $ fromJust (decode requestBody :: Maybe UserIdRequest)
                    -- Right ("n1","s1",False)
                    -- Left (QueryError "SELECT name :: text, surname :: text, is_admin :: bool FROM users WHERE user_id = $1 :: int2" ["-231"] (ResultError (UnexpectedAmountOfRows 0)))
                    pure . fromStrict . pack $ show sessionResults
                "deleteUser" -> do
                    sessionResults <- HSS.deleteUser $ fromJust (decode requestBody :: Maybe UserIdRequest)
                    pure . fromStrict . pack $ show sessionResults
                _ -> pure "no such method"
            --respond $ responseLBS H.status200 [] errorOrSessionName)
            let {
                httpStatus = case errorOrSessionName of
                    "No such endpoint" -> H.status404
                    "Wrong parameters/parameters values" -> H.status400
                    "Method is not implemented" -> H.status501
                    _ -> H.status200;
            } in respond $ responseLBS httpStatus [] results)


runWarp :: IO ()
runWarp = let {
    port = 8081 :: Port;
} in run port restAPI >> pure ()

runWarpWithLogger :: IO ()
runWarpWithLogger = runWarp
