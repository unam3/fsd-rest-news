{-# LANGUAGE OverloadedStrings  #-}

module RestNews
    ( runWarpWithLogger
    ) where

import AesonDefinitions (CreateUserRequest, GetUserRequest)
import qualified HasqlSessions as HSS

import Control.Exception (bracket_)
import Data.Aeson (decode)
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.ByteString.Char8 (pack)
--import Data.ByteString.Lazy (fromStrict)
import Data.Maybe (fromJust)
import qualified Hasql.Session as Session
import qualified Network.HTTP.Types as H
-- https://hackage.haskell.org/package/http-types-0.12.3/docs/Network-HTTP-Types-Method.html#t:Method
import Network.HTTP.Types.Method (methodGet, methodPost)
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

            errorOrSessionName <- let {
                maybeCreateUserRequestJSON = decode requestBody :: Maybe CreateUserRequest;
                maybeGetUserRequestJSON = decode requestBody :: Maybe GetUserRequest;
            } in pure (
                if isPathHasEndpoint
                    then (case pathHeadChunk of
                        "authors" -> case requestMethod request of
                            methodPost -> case maybeCreateUserRequestJSON of
                                Just createUserRequest -> "createUser"
                                Nothing -> "Wrong parameters/values"
                            methodGet -> case maybeGetUserRequestJSON of
                                Just getUserRequest -> "getUser"
                                Nothing -> "Wrong parameters/values"
                        _ -> show pathTextChunks)
                    else "No such endpoint")

            results <- case errorOrSessionName of
                "createUser" -> HSS.createUser $ fromJust (decode requestBody :: Maybe CreateUserRequest)
                "getUser" -> HSS.getUser $ fromJust (decode requestBody :: Maybe GetUserRequest)
                _ -> pure . Left $
                    Session.QueryError (pack "foo") ["pluh"] (Session.ClientError Nothing)
            --    dbReturnsOk = (Session.ResultError (Session.UnexpectedResult "Unexpected result status: CommandOk"));
            --respond $ responseLBS H.status200 [] errorOrSessionName)
            respond $ responseLBS H.status200 [] (fromString $ show results))


runWarp :: IO ()
runWarp = let {
    port = 8081 :: Port;
} in run port restAPI >> pure ()

runWarpWithLogger :: IO ()
runWarpWithLogger = runWarp
