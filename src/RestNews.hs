{-# LANGUAGE OverloadedStrings  #-}

module RestNews
    ( runWarpWithLogger
    ) where

import AesonDefinitions (CreateUserRequest, UserIdRequest, PromoteUserToAuthorRequest, EditAuthorRequest, AuthorIdRequest, CreateCategoryRequest, UpdateCategoryRequest, CategoryIdRequest, CreateTagRequest, EditTagRequest, TagIdRequest, CreateCommentRequest, CreateCommentRequest, CommentIdRequest, ArticleCommentsRequest, ArticleDraftRequest, ArticleDraftIdRequest, ArticlesByCategoryIdRequest, ArticlesByTagIdListRequest, ArticlesByTitlePartRequest)
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
                            _ -> "No such endpoint"
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
                "promoteUserToAuthor" -> do
                    sessionResults <- HSS.promoteUserToAuthor $ fromJust (decode requestBody :: Maybe PromoteUserToAuthorRequest)
                    pure . fromStrict . pack $ show sessionResults
                "editAuthor" -> do
                    sessionResults <- HSS.editAuthor $ fromJust (decode requestBody :: Maybe EditAuthorRequest)
                    pure . fromStrict . pack $ show sessionResults
                "getAuthor" -> do
                    sessionResults <- HSS.getAuthor $ fromJust (decode requestBody :: Maybe AuthorIdRequest)
                    pure . fromStrict . pack $ show sessionResults
                "deleteAuthorRole" -> do
                    sessionResults <- HSS.deleteAuthorRole $ fromJust (decode requestBody :: Maybe AuthorIdRequest)
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
                "createArticleDraft" -> do
                    sessionResults <- HSS.createArticleDraft $ fromJust (decode requestBody :: Maybe ArticleDraftRequest)
                    pure . fromStrict . pack $ show sessionResults
                "publishArticleDraft" -> do
                    sessionResults <- HSS.publishArticleDraft $ fromJust (decode requestBody :: Maybe ArticleDraftIdRequest)
                    pure . fromStrict . pack $ show sessionResults
                "getArticleDraft" -> do
                    sessionResults <- HSS.getArticleDraft $ fromJust (decode requestBody :: Maybe ArticleDraftIdRequest)
                    pure . fromStrict . pack $ show sessionResults
                "getArticlesByCategoryId" -> do
                    sessionResults <- HSS.getArticlesByCategoryId $ fromJust (decode requestBody :: Maybe ArticlesByCategoryIdRequest)
                    pure . fromStrict . pack $ show sessionResults
                "getArticlesByTagId" -> do
                    sessionResults <- HSS.getArticlesByTagId $ fromJust (decode requestBody :: Maybe TagIdRequest)
                    pure . fromStrict . pack $ show sessionResults
                "getArticlesByAnyTagId" -> do
                    sessionResults <- HSS.getArticlesByAnyTagId $ fromJust (decode requestBody :: Maybe ArticlesByTagIdListRequest)
                    pure . fromStrict . pack $ show sessionResults
                "getArticlesByAllTagId" -> do
                    sessionResults <- HSS.getArticlesByAllTagId $ fromJust (decode requestBody :: Maybe ArticlesByTagIdListRequest)
                    pure . fromStrict . pack $ show sessionResults
                "getArticlesByTitlePart" -> do
                    sessionResults <- HSS.getArticlesByTitlePart $ fromJust (decode requestBody :: Maybe ArticlesByTitlePartRequest)
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
