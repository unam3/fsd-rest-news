{-# LANGUAGE OverloadedStrings  #-}

module RestNews
    ( runWarpWithLogger
    ) where

import AesonDefinitions (CreateUserRequest, UserIdRequest, PromoteUserToAuthorRequest, EditAuthorRequest, AuthorIdRequest, CreateCategoryRequest, UpdateCategoryRequest, CategoryIdRequest, CreateTagRequest, EditTagRequest, TagIdRequest, CreateCommentRequest, CreateCommentRequest, CommentIdRequest, ArticleCommentsRequest, ArticleDraftRequest, ArticleDraftIdRequest, ArticlesByCategoryIdRequest, ArticlesByTagIdListRequest, ArticlesByTitlePartRequest, ArticlesByContentPartRequest, ArticlesByAuthorNamePartRequest)
import qualified HasqlSessions as HSS

import Control.Exception (bracket_)
import Control.Monad (void, when)
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.UTF8 as UTFLBS
import Data.Maybe (fromJust, isJust)
import qualified Data.Vault.Lazy as Vault
import Database.PostgreSQL.Simple
import qualified Network.HTTP.Types as H
import Network.Wai (Application, pathInfo, requestMethod, responseLBS, strictRequestBody, vault)
import Network.Wai.Handler.Warp (Port, run)
import Network.Wai.Session (withSession, Session)
import Network.Wai.Session.PostgreSQL (dbStore, defaultSettings, fromSimpleConnection, purger)
import System.Log.Logger (Priority (DEBUG, ERROR), debugM, setLevel, traplogging, updateGlobalLogger)
import Web.Cookie (defaultSetCookie)

ifValidRequest :: String -> Maybe a -> String
ifValidRequest sessionName = maybe "Wrong parameters/parameters values" (const sessionName)

dbconnect :: IO Connection
dbconnect = let {
    connectInfo = ConnectInfo {
          connectHost = "localhost"
        , connectPort = 5432
        , connectUser = "rest-news-user"
        , connectPassword = "rest"
        , connectDatabase = "rest-news-db" };
    } in connectPostgreSQL $ postgreSQLConnectionString connectInfo


--type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
restAPI :: Vault.Key (Session IO String String) -> Application;
restAPI vaultKey request respond = let {
        pathTextChunks = pathInfo request;
        isRequestPathNotEmpty = (not $ null pathTextChunks);
        pathHeadChunk = head pathTextChunks;
        method = requestMethod request;
    } in bracket_
        (debugM "rest-news" "Allocating scarce resource")
        (debugM "rest-news" "Cleaning up")
        (let {
            Just (sessionLookup, sessionInsert) = Vault.lookup vaultKey (vault request);
        } in do
            requestBody <- strictRequestBody request

            sessionIsAdmin <- sessionLookup "is_admin"
            debugM "rest-news" $ show ("is_admin", sessionIsAdmin)
            
            when (pathHeadChunk == "login") $ do
                putStrLn "login simulation"
                sessionInsert "is_admin" "True"

            debugM "rest-news" (show (method, pathTextChunks, requestBody))
            errorOrSessionName <- let {
                isAdmin (Just _) = True;
                isAdmin _ = False;
                sessionNameOr404 sessionName = if isAdmin sessionIsAdmin
                    then sessionName
                    else "No such endpoint";
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
                maybeArticlesByAuthorNamePartRequest = decode requestBody :: Maybe ArticlesByAuthorNamePartRequest;
            } in pure (
                if isRequestPathNotEmpty
                    then (case pathHeadChunk of
                        "users" -> case method of
                            "POST"      -> ifValidRequest "createUser" maybeCreateUserRequestJSON
                            "GET"       -> ifValidRequest "getUser" maybeUserIdRequestJSON
                            "DELETE"    -> sessionNameOr404 $ ifValidRequest "deleteUser" maybeUserIdRequestJSON
                            _ -> "Method is not implemented"
                        "authors" -> case method of
                            "POST"      -> ifValidRequest "promoteUserToAuthor" maybePromoteUserToAuthorRequestJSON
                            "PATCH"     -> ifValidRequest "editAuthor" maybeEditAuthorRequestJSON
                            "GET"       -> ifValidRequest "getAuthor" maybeAuthorIdRequestJSON
                            "DELETE"    -> ifValidRequest "deleteAuthorRole" maybeAuthorIdRequestJSON
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
                            ["in__author_name"] -> case method of
                                "GET" -> ifValidRequest "getArticlesByAuthorNamePart" maybeArticlesByAuthorNamePartRequest
                                _ -> "Method is not implemented"
                            _ -> "No such endpoint"
                        _ -> "No such endpoint")
                    else "Endpoint needed")

            results <- let {
                runSession session = (session . fromJust $ decode requestBody);
                sessionResults = case errorOrSessionName of
                    "createUser" -> runSession HSS.createUser
                    "getUser" -> runSession HSS.getUser
                    "deleteUser" -> runSession HSS.deleteUser
                    "promoteUserToAuthor" -> runSession HSS.promoteUserToAuthor;
                    "editAuthor" -> runSession HSS.editAuthor
                    "getAuthor" -> runSession HSS.getAuthor
                    "deleteAuthorRole" -> runSession HSS.deleteAuthorRole
                    "createCategory" -> runSession HSS.createCategory
                    "updateCategory" -> runSession HSS.updateCategory
                    "getCategory" -> runSession HSS.getCategory
                    "deleteCategory" -> runSession HSS.deleteCategory
                    "createTag" -> runSession HSS.createTag
                    "editTag" -> runSession HSS.editTag
                    "getTag" -> runSession HSS.getTag
                    "deleteTag" -> runSession HSS.deleteTag
                    "createComment" -> runSession HSS.createComment
                    "deleteComment" -> runSession HSS.deleteComment
                    "getArticleComments" -> runSession HSS.getArticleComments
                    "createArticleDraft" -> runSession HSS.createArticleDraft
                    "publishArticleDraft" -> runSession HSS.publishArticleDraft
                    "getArticleDraft" -> runSession HSS.getArticleDraft
                    "getArticlesByCategoryId" -> runSession HSS.getArticlesByCategoryId
                    "getArticlesByTagId" -> runSession HSS.getArticlesByTagId
                    "getArticlesByAnyTagId" -> runSession HSS.getArticlesByAnyTagId
                    "getArticlesByAllTagId" -> runSession HSS.getArticlesByAllTagId
                    "getArticlesByTitlePart" -> runSession HSS.getArticlesByTitlePart
                    "getArticlesByContentPart" -> runSession HSS.getArticlesByContentPart
                    "getArticlesByAuthorNamePart" -> runSession HSS.getArticlesByAuthorNamePart
                    nonMatched -> pure . pure $ UTFLBS.fromString nonMatched;
                } in sessionResults
            resultsString <- pure (case results of
                (Right ulbs) -> UTFLBS.toString ulbs
                leftErr -> show leftErr)
            debugM "rest-news" resultsString

            processedResults <- pure (case results of
                (Right ulbs) -> ulbs
                _ -> "left sth" :: UTFLBS.ByteString)
            let {
                httpStatus = (case errorOrSessionName of
                    "Endpoint needed" -> H.status404
                    "No such endpoint" -> H.status404
                    "Wrong parameters/parameters values" -> H.status400
                    "Method is not implemented" -> H.status501
                    _ -> H.status200);
            } in respond $ responseLBS httpStatus [] processedResults)


runWarp :: IO ()
runWarp = let {
    port = 8081 :: Port;
} in do
    vaultK <- Vault.newKey
    simpleConnection <- (dbconnect >>= fromSimpleConnection)
    -- IO (SessionStore IO String String)
    store <- dbStore simpleConnection defaultSettings
    void (purger simpleConnection defaultSettings)
    void (run port
        -- :: SessionStore m k v	The SessionStore to use for sessions
        -- -> ByteString	        Name to use for the session cookie (MUST BE ASCII)
        -- -> SetCookie	            Settings for the cookie (path, expiry, etc)
        -- -> Key (Session m k v)	Vault key to use when passing the session through
        -- -> Middleware	 
        $ withSession store "SESSION" defaultSetCookie vaultK
        $ restAPI vaultK)

runWarpWithLogger :: IO ()
runWarpWithLogger = traplogging "rest-news" ERROR "shutdown due to"
    $ updateGlobalLogger "rest-news" (setLevel DEBUG)
    >> runWarp
