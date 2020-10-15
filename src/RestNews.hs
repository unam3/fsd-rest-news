{-# LANGUAGE OverloadedStrings  #-}

module RestNews
    ( runWarpWithLogger
    ) where

import AesonDefinitions (CreateUserRequest, UserIdRequest, PromoteUserToAuthorRequest, EditAuthorRequest, AuthorIdRequest, CreateCategoryRequest, UpdateCategoryRequest, CategoryIdRequest, CreateTagRequest, EditTagRequest, TagIdRequest, CreateCommentRequest, CreateCommentRequest, CommentIdRequest, ArticleCommentsRequest, ArticleDraftRequest, ArticleDraftIdRequest, ArticlesByCategoryIdRequest, ArticlesByTagIdListRequest, ArticlesByTitlePartRequest, ArticlesByContentPartRequest)
import qualified HasqlSessions as HSS

import Control.Exception (bracket_)
import Data.Aeson (decode)
--import Data.ByteString.Lazy.UTF8 (fromString)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (fromStrict)
import Data.Maybe (fromJust, isJust)
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
        method = requestMethod request;
    } in bracket_
        (debugM "rest-news" "Allocating scarce resource")
        (debugM "rest-news" "Cleaning up")
        (do
            requestBody <- strictRequestBody request
            print (method, pathTextChunks, requestBody)

            errorOrSessionName <- let {
                maybeCreateUserRequestJSON = decode requestBody :: Maybe CreateUserRequest;
                maybeUserIdRequestJSON = decode requestBody :: Maybe UserIdRequest;
                maybePromoteUserToAuthorRequestJSON = decode requestBody :: Maybe PromoteUserToAuthorRequest;
                maybeEditAuthorRequestJSON = decode requestBody :: Maybe EditAuthorRequest;
                maybeAuthorIdRequestJSON = decode requestBody :: Maybe AuthorIdRequest;
                maybeCreateCategoryRequestJSON = decode requestBody :: Maybe CreateCategoryRequest;
                maybeUpdateCategoryRequestJSON = decode requestBody :: Maybe UpdateCategoryRequest;
                maybeCategoryIdRequestJSON = decode requestBody :: Maybe CategoryIdRequest;
                maybeCreateTagRequestJSON = decode requestBody :: Maybe CreateTagRequest;
                maybeEditTagRequestJSON = decode requestBody :: Maybe EditTagRequest;
                maybeTagIdRequestJSON = decode requestBody :: Maybe TagIdRequest;
                maybeCreateCommentRequestJSON = decode requestBody :: Maybe CreateCommentRequest;
                maybeCommentIdRequestJSON = decode requestBody :: Maybe CommentIdRequest;
                maybeArticleCommentsRequestJSON = decode requestBody :: Maybe ArticleCommentsRequest;
                maybeArticleDraftRequestJSON = decode requestBody :: Maybe ArticleDraftRequest;
                maybeArticleDraftIdRequestJSON = decode requestBody :: Maybe ArticleDraftIdRequest;
                maybeArticlesByCategoryIdRequestJSON = decode requestBody :: Maybe ArticlesByCategoryIdRequest;
                maybeArticlesByTagIdListRequest = decode requestBody :: Maybe ArticlesByTagIdListRequest;
                maybeArticlesByTitlePartRequest = decode requestBody :: Maybe ArticlesByTitlePartRequest;
                maybeArticlesByContentPartRequest = decode requestBody :: Maybe ArticlesByContentPartRequest;
            } in pure (
                if isRequestPathNotEmpty
                    then (case pathHeadChunk of
                        "users" -> case method of
                            "POST"      -> ifValidRequest "createUser" maybeCreateUserRequestJSON
                            "GET"       -> ifValidRequest "getUser" maybeUserIdRequestJSON
                            "DELETE"    -> ifValidRequest "deleteUser" maybeUserIdRequestJSON
                            _ -> "Method is not implemented"
                        "authors" -> case method of
                            "POST"      -> ifValidRequest "promoteUserToAuthor" maybePromoteUserToAuthorRequestJSON
                            "PATCH"     -> ifValidRequest "editAuthor" maybeEditAuthorRequestJSON
                            "GET"       -> ifValidRequest "getAuthor" maybeAuthorIdRequestJSON
                            "DELETE"    -> ifValidRequest "deleteAuthorRole" maybeUserIdRequestJSON
                            _ -> "Method is not implemented"
                        "categories" -> case method of
                            "POST"      -> ifValidRequest "createCategory" maybeCreateCategoryRequestJSON
                            "PATCH"     -> ifValidRequest "updateCategory" maybeUpdateCategoryRequestJSON
                            "GET"       -> ifValidRequest "getCategory" maybeCategoryIdRequestJSON
                            "DELETE"    -> ifValidRequest "deleteCategory" maybeCategoryIdRequestJSON
                            _ -> "Method is not implemented"
                        "tags" -> case method of
                            "POST"      -> ifValidRequest "createTag" maybeCreateTagRequestJSON
                            "PATCH"     -> ifValidRequest "editTag" maybeEditTagRequestJSON
                            "GET"       -> ifValidRequest "getTag" maybeTagIdRequestJSON
                            "DELETE"    -> ifValidRequest "deleteTag" maybeTagIdRequestJSON
                            _ -> "Method is not implemented"
                        "comments" -> case method of
                            "POST"      -> ifValidRequest "createComment" maybeCreateCommentRequestJSON
                            "GET"       -> ifValidRequest "getArticleComments" maybeArticleCommentsRequestJSON
                            "DELETE"    -> ifValidRequest "deleteComment" maybeCommentIdRequestJSON
                            _ -> "Method is not implemented"
                        "articles" -> case tail pathTextChunks of
                            [] -> case method of
                                "POST" -> if isJust maybeArticleDraftRequestJSON
                                    then "createArticleDraft"
                                    else ifValidRequest "publishArticleDraft" maybeArticleDraftIdRequestJSON
                                "GET" -> ifValidRequest "getArticleDraft" maybeArticleDraftIdRequestJSON
                                _ -> "Method is not implemented"
                            ["category"] -> case method of
                                "GET" -> ifValidRequest "getArticlesByCategoryId" maybeArticlesByCategoryIdRequestJSON
                                _ -> "Method is not implemented"
                            ["tag"] -> case method of
                                "GET" -> ifValidRequest "getArticlesByTagId" maybeTagIdRequestJSON
                                _ -> "Method is not implemented"
                            ["tags__any"] -> case method of
                                "GET" -> ifValidRequest "getArticlesByAnyTagId" maybeArticlesByTagIdListRequest
                                _ -> "Method is not implemented"
                            ["tags__all"] -> case method of
                                "GET" -> ifValidRequest "getArticlesByAllTagId" maybeArticlesByTagIdListRequest
                                _ -> "Method is not implemented"
                            ["in__title"] -> case method of
                                "GET" -> ifValidRequest "getArticlesByTitlePart" maybeArticlesByTitlePartRequest
                                _ -> "Method is not implemented"
                            ["in__content"] -> case method of
                                "GET" -> ifValidRequest "getArticlesByContentPart" maybeArticlesByContentPartRequest
                                _ -> "Method is not implemented"
                            _ -> "No such endpoint"
                        _ -> "No such endpoint")
                    else "Endpoint needed")

            results <- let {
                --runSession :: Text -> IO 
                runSession session = (session . fromJust $ decode requestBody) >>= (pure . fromStrict . pack . show);

                sessionResults = case errorOrSessionName of
                --"createUser" -> do
                --    sessionResults <- HSS.createUser $ fromJust (decode requestBody :: Maybe CreateUserRequest)
                --    pure $ case sessionResults of
                --        Left (Session.QueryError _ _ (Session.ResultError (Session.UnexpectedResult "Unexpected result status: CommandOk"))) -> "ok"
                --        Left (Session.QueryError _ _ (Session.ResultError sessionError)) -> fromStrict . pack $ show sessionError
                --        _ -> "eeh?"

                "getUser" -> runSession HSS.getUser
                "deleteUser" -> runSession HSS.deleteUser
                "promoteUserToAuthor" -> runSession HSS.promoteUserToAuthor;
                nonMatched -> pure . fromStrict . pack $ nonMatched
            } in sessionResults
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
