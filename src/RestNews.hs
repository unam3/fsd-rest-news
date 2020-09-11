{-# LANGUAGE OverloadedStrings  #-}

module RestNews
    ( runWarpWithLogger
    ) where

import AesonDefinitions (CreateUserRequest, UserIdRequest, CreateCategoryRequest, UpdateCategoryRequest, CategoryIdRequest, CreateTagRequest, EditTagRequest, TagIdRequest, CreateCommentRequest, CreateCommentRequest, CommentIdRequest, ArticleCommentsRequest)
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

ifValidRequest :: String -> Maybe a -> String
ifValidRequest sessionName = maybe "Wrong parameters/parameters values" (const sessionName)

--type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
restAPI :: Application;
restAPI request respond = let {
        pathTextChunks = pathInfo request;
        isRequestPathNotEmpty = (not $ null pathTextChunks);
        pathHeadChunk = head pathTextChunks;
    } in bracket_
        (debugM "rest-news" "Allocating scarce resource")
        (debugM "rest-news" "Cleaning up")
        (do
            requestBody <- strictRequestBody request
            print requestBody

            errorOrSessionName <- let {
                maybeCreateUserRequestJSON = decode requestBody :: Maybe CreateUserRequest;
                maybeUserIdRequestJSON = decode requestBody :: Maybe UserIdRequest;
                maybeCreateCategoryRequestJSON = decode requestBody :: Maybe CreateCategoryRequest;
                maybeUpdateCategoryRequestJSON = decode requestBody :: Maybe UpdateCategoryRequest;
                maybeCategoryIdRequestJSON = decode requestBody :: Maybe CategoryIdRequest;
                maybeCreateTagRequestJSON = decode requestBody :: Maybe CreateTagRequest;
                maybeEditTagRequestJSON = decode requestBody :: Maybe EditTagRequest;
                maybeTagIdRequestJSON = decode requestBody :: Maybe TagIdRequest;
                maybeCreateCommentRequestJSON = decode requestBody :: Maybe CreateCommentRequest;
                maybeCommentIdRequestJSON = decode requestBody :: Maybe CommentIdRequest;
                maybeArticleCommentsRequestJSON = decode requestBody :: Maybe ArticleCommentsRequest;
            } in pure (
                if isRequestPathNotEmpty
                    then (case pathHeadChunk of
                        "authors" -> case requestMethod request of
                            "POST"      -> ifValidRequest "createUser" maybeCreateUserRequestJSON
                            "GET"       -> ifValidRequest "getUser" maybeUserIdRequestJSON
                            "DELETE"    -> ifValidRequest "deleteUser" maybeUserIdRequestJSON
                            _ -> "Method is not implemented"
                        "categories" -> case requestMethod request of
                            "POST"      -> ifValidRequest "createCategory" maybeCreateCategoryRequestJSON
                            "PATCH"     -> ifValidRequest "updateCategory" maybeUpdateCategoryRequestJSON
                            "GET"       -> ifValidRequest "getCategory" maybeCategoryIdRequestJSON
                            "DELETE"    -> ifValidRequest "deleteCategory" maybeCategoryIdRequestJSON
                            _ -> "Method is not implemented"
                        "tags" -> case requestMethod request of
                            "POST"      -> ifValidRequest "createTag" maybeCreateTagRequestJSON
                            "PATCH"     -> ifValidRequest "editTag" maybeEditTagRequestJSON
                            "GET"       -> ifValidRequest "getTag" maybeTagIdRequestJSON
                            "DELETE"    -> ifValidRequest "deleteTag" maybeTagIdRequestJSON
                            _ -> "Method is not implemented"
                        "comments" -> case requestMethod request of
                            "POST"      -> ifValidRequest "createComment" maybeCreateCommentRequestJSON
                            "GET"       -> ifValidRequest "getArticleComments" maybeArticleCommentsRequestJSON
                            "DELETE"    -> ifValidRequest "deleteComment" maybeCommentIdRequestJSON
                            _ -> "Method is not implemented"
                        _ -> "No such endpoint")
                    else "Endpoint needed")

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
                "createCategory" -> do
                    sessionResults <- HSS.createCategory $ fromJust (decode requestBody :: Maybe CreateCategoryRequest)
                    pure . fromStrict . pack $ show sessionResults
                "updateCategory" -> do
                    sessionResults <- HSS.updateCategory $ fromJust (decode requestBody :: Maybe UpdateCategoryRequest)
                    pure . fromStrict . pack $ show sessionResults
                "getCategory" -> do
                    sessionResults <- HSS.getCategory $ fromJust (decode requestBody :: Maybe CategoryIdRequest)
                    pure . fromStrict . pack $ show sessionResults
                "deleteCategory" -> do
                    sessionResults <- HSS.deleteCategory $ fromJust (decode requestBody :: Maybe CategoryIdRequest)
                    pure . fromStrict . pack $ show sessionResults
                "createTag" -> do
                    sessionResults <- HSS.createTag $ fromJust (decode requestBody :: Maybe CreateTagRequest)
                    pure . fromStrict . pack $ show sessionResults
                "editTag" -> do
                    sessionResults <- HSS.editTag $ fromJust (decode requestBody :: Maybe EditTagRequest)
                    pure . fromStrict . pack $ show sessionResults
                "getTag" -> do
                    sessionResults <- HSS.getTag $ fromJust (decode requestBody :: Maybe TagIdRequest)
                    pure . fromStrict . pack $ show sessionResults
                "deleteTag" -> do
                    sessionResults <- HSS.deleteTag $ fromJust (decode requestBody :: Maybe TagIdRequest)
                    pure . fromStrict . pack $ show sessionResults
                "createComment" -> do
                    sessionResults <- HSS.createComment $ fromJust (decode requestBody :: Maybe CreateCommentRequest)
                    pure . fromStrict . pack $ show sessionResults
                "deleteComment" -> do
                    sessionResults <- HSS.deleteComment $ fromJust (decode requestBody :: Maybe CommentIdRequest)
                    pure . fromStrict . pack $ show sessionResults
                "getArticleComments" -> do
                    sessionResults <- HSS.getArticleComments $ fromJust (decode requestBody :: Maybe ArticleCommentsRequest)
                    pure . fromStrict . pack $ show sessionResults
                nonMatched -> pure . fromStrict . pack $ nonMatched
            --respond $ responseLBS H.status200 [] errorOrSessionName)
            let {
                httpStatus = case errorOrSessionName of
                    "Endpoint needed" -> H.status404
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
