{-# LANGUAGE OverloadedStrings  #-}

module RestNews
    ( runWarpWithLogger
    ) where

import AesonDefinitions (AuthRequest, CreateUserRequest, UserIdRequest, PromoteUserToAuthorRequest, EditAuthorRequest, AuthorIdRequest, CreateCategoryRequest, UpdateCategoryRequest, CategoryIdRequest, CreateTagRequest, EditTagRequest, TagIdRequest, TagIdRequestWithOffset, CreateCommentRequest, CreateCommentRequest, CommentIdRequest, ArticleCommentsRequest, ArticleDraftRequest, ArticleDraftEditRequest, ArticleDraftIdRequest, ArticlesByCategoryIdRequest, ArticlesByTagIdListRequest, ArticlesByTitlePartRequest, ArticlesByContentPartRequest, ArticlesByAuthorNamePartRequest, ArticlesByCreationDateRequest, OffsetRequest)
import qualified HasqlSessions as HSS

import Control.Concurrent (forkIO)
import Control.Exception (bracket_)
import Control.Monad (void, when)
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.UTF8 as UTFLBS
import Data.Either (fromRight)
import Data.Int (Int32)
import Data.Maybe (fromJust, isJust)
import qualified Data.Vault.Lazy as Vault
import Database.PostgreSQL.Simple (Connection, ConnectInfo(..), connectPostgreSQL, postgreSQLConnectionString)
import Hasql.Session (QueryError)
import qualified Network.HTTP.Types as H
import Network.Wai (Application, Request, pathInfo, requestMethod, responseLBS, strictRequestBody, vault)
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp (Port, run)
import Network.Wai.Session (withSession, Session)
import Network.Wai.Session.PostgreSQL (clearSession, dbStore, defaultSettings, fromSimpleConnection, purger)
import System.Log.Logger (Priority (DEBUG, ERROR), debugM, setLevel, traplogging, updateGlobalLogger)
import Web.Cookie (defaultSetCookie)

ifValidRequest :: String -> Maybe a -> String
ifValidRequest sessionName = maybe "Wrong parameters/parameters values" (const sessionName)

getIdString :: Maybe String -> String
getIdString = maybe "0" id

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
restAPI :: Vault.Key (Session IO String String) -> (Request -> IO ()) -> Application;
restAPI vaultKey clearSessionPartial request respond = let {
        pathTextChunks = pathInfo request;
        isRequestPathNotEmpty = (not $ null pathTextChunks);
        pathHeadChunk = head pathTextChunks;
        method = requestMethod request;
        Just (sessionLookup, sessionInsert) = Vault.lookup vaultKey (vault request);
        processCredentials :: Either QueryError (Int32, Bool, Int32) -> IO (Either QueryError UTFLBS.ByteString);
        processCredentials sessionResults = let {
            (user_id, is_admin, author_id) = fromRight (0, False, 0) sessionResults;
        } in
            -- clearSession will fail if request has no associated session with cookies:
            -- https://github.com/hce/postgresql-session/blob/master/src/Network/Wai/Session/PostgreSQL.hs#L232
            (do
                session_user_id <- sessionLookup "user_id"
                when
                    (session_user_id /= Nothing)
                    (debugM "rest-news" "invalidating session"
                        >> clearSessionPartial request)
                )
            >> (debugM "rest-news" $ show ("put into sessions:" :: String, user_id, is_admin, author_id))
            >> (sessionInsert "is_admin" $ show is_admin)
            >> (sessionInsert "user_id" $ show user_id)
            >> (sessionInsert "author_id" $ show author_id)
            >> (pure $ fmap (const "cookies are baked") sessionResults);
    } in bracket_
        (debugM "rest-news" "Allocating scarce resource")
        (debugM "rest-news" "Cleaning up")
        (do
            maybeUserId <- sessionLookup "user_id"
            maybeIsAdmin <- sessionLookup "is_admin"
            maybeAuthorId <- sessionLookup "author_id"
            let sessionUserId = getIdString maybeUserId
            let sessionAuthorIdString = getIdString maybeAuthorId

            debugM "rest-news" $ show request
            debugM "rest-news" $ show ("session user_id" :: String, maybeUserId)
            debugM "rest-news" $ show ("session is_admin" :: String, maybeIsAdmin)
            debugM "rest-news" $ show ("session author_id" :: String, maybeAuthorId)

            requestBody <- strictRequestBody request
            
            debugM "rest-news" (show (method, pathTextChunks, requestBody))

            errorOrSessionName <- let {
                isAdmin (Just "True") = True;
                isAdmin _ = False;
                passSessionNameIfAdmin sessionName = if isAdmin maybeIsAdmin
                    then sessionName
                    else "No such endpoint";
                passIfHasUserId sessionName = if sessionUserId /= "0"
                    then sessionName
                    else "No such endpoint";
                passIfHasAuthorId sessionName = if sessionAuthorIdString /= "0"
                    then sessionName
                    else "No such endpoint";
                maybeAuthRequestJSON = decode requestBody :: Maybe AuthRequest;
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
                maybeTagIdRequestWithOffsetJSON = decode requestBody :: Maybe TagIdRequestWithOffset;
                maybeCreateCommentRequestJSON = decode requestBody :: Maybe CreateCommentRequest;
                maybeCommentIdRequestJSON = decode requestBody :: Maybe CommentIdRequest;
                maybeArticleCommentsRequestJSON = decode requestBody :: Maybe ArticleCommentsRequest;
                maybeArticleDraftRequestJSON = decode requestBody :: Maybe ArticleDraftRequest;
                maybeArticleDraftEditRequestJSON = decode requestBody :: Maybe ArticleDraftEditRequest;
                maybeArticleDraftIdRequestJSON = decode requestBody :: Maybe ArticleDraftIdRequest;
                maybeArticlesByCategoryIdRequestJSON = decode requestBody :: Maybe ArticlesByCategoryIdRequest;
                maybeArticlesByTagIdListRequest = decode requestBody :: Maybe ArticlesByTagIdListRequest;
                maybeArticlesByTitlePartRequest = decode requestBody :: Maybe ArticlesByTitlePartRequest;
                maybeArticlesByContentPartRequest = decode requestBody :: Maybe ArticlesByContentPartRequest;
                maybeArticlesByAuthorNamePartRequest = decode requestBody :: Maybe ArticlesByAuthorNamePartRequest;
                maybeArticlesFilteredByCreationDate = decode requestBody :: Maybe ArticlesByCreationDateRequest;
                maybeOffsetRequest = decode requestBody :: Maybe OffsetRequest;
            } in pure (
                if isRequestPathNotEmpty
                    then (case pathHeadChunk of
                        "auth" -> case method of
                            "POST"      -> ifValidRequest "auth" maybeAuthRequestJSON
                            _ -> "Method is not implemented"
                        "users" -> case method of
                            "POST"      -> ifValidRequest "createUser" maybeCreateUserRequestJSON
                            "GET"       -> passIfHasUserId "getUser"
                            "DELETE"    -> passSessionNameIfAdmin   
                                $ ifValidRequest "deleteUser" maybeUserIdRequestJSON
                            _ -> "Method is not implemented"
                        "authors" -> passSessionNameIfAdmin $ case method of
                            "POST"      -> ifValidRequest "promoteUserToAuthor" maybePromoteUserToAuthorRequestJSON
                            "PATCH"     -> ifValidRequest "editAuthor" maybeEditAuthorRequestJSON
                            "GET"       -> ifValidRequest "getAuthor" maybeAuthorIdRequestJSON
                            "DELETE"    -> ifValidRequest "deleteAuthorRole" maybeAuthorIdRequestJSON
                            _ -> "Method is not implemented"
                        "categories" -> case method of
                            "POST"      -> passSessionNameIfAdmin
                                $ ifValidRequest "createCategory" maybeCreateCategoryRequestJSON
                            "PATCH"     -> passSessionNameIfAdmin
                                $ ifValidRequest "updateCategory" maybeUpdateCategoryRequestJSON
                            "GET"       -> ifValidRequest "getCategory" maybeCategoryIdRequestJSON
                            "DELETE"    -> passSessionNameIfAdmin
                                $ ifValidRequest "deleteCategory" maybeCategoryIdRequestJSON
                            _ -> "Method is not implemented"
                        "tags" -> case method of
                            "POST"      -> passSessionNameIfAdmin $ ifValidRequest "createTag" maybeCreateTagRequestJSON
                            "PATCH"     -> passSessionNameIfAdmin $ ifValidRequest "editTag" maybeEditTagRequestJSON
                            "GET"       -> ifValidRequest "getTag" maybeTagIdRequestJSON
                            "DELETE"    -> passSessionNameIfAdmin $ ifValidRequest "deleteTag" maybeTagIdRequestJSON
                            _ -> "Method is not implemented"
                        "comments" -> case method of
                            "POST"      -> passIfHasUserId
                                $ ifValidRequest "createComment" maybeCreateCommentRequestJSON
                            "GET"       -> ifValidRequest "getArticleComments" maybeArticleCommentsRequestJSON
                            "DELETE"    -> passIfHasUserId
                                $ ifValidRequest "deleteComment" maybeCommentIdRequestJSON
                            _ -> "Method is not implemented"
                        "articles" -> case tail pathTextChunks of
                            [] -> passIfHasAuthorId $ case method of
                                "POST" -> if isJust maybeArticleDraftRequestJSON
                                    then "createArticleDraft"
                                    else ifValidRequest "publishArticleDraft" maybeArticleDraftIdRequestJSON
                                "PATCH" -> ifValidRequest "editArticleDraft" maybeArticleDraftEditRequestJSON
                                "GET" -> ifValidRequest "getArticleDraft" maybeArticleDraftIdRequestJSON
                                "DELETE" -> ifValidRequest "deleteArticleDraft" maybeArticleDraftIdRequestJSON
                                _ -> "Method is not implemented"
                            ["category"] -> case method of
                                "GET" -> ifValidRequest "getArticlesByCategoryId" maybeArticlesByCategoryIdRequestJSON
                                _ -> "Method is not implemented"
                            ["tag"] -> case method of
                                "GET" -> ifValidRequest "getArticlesByTagId" maybeTagIdRequestWithOffsetJSON
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
                                "GET" -> ifValidRequest
                                    "getArticlesByAuthorNamePart"
                                    maybeArticlesByAuthorNamePartRequest
                                _ -> "Method is not implemented"
                            ["byPhotosNumber"] -> case method of
                                "GET" -> ifValidRequest
                                    "getArticlesSortedByPhotosNumber"
                                    maybeOffsetRequest
                                _ -> "Method is not implemented"
                            ["byCreationDate"] -> case method of
                                "GET" -> ifValidRequest
                                    "getArticlesSortedByCreationDate"
                                    maybeOffsetRequest
                                _ -> "Method is not implemented"
                            ["sortByAuthor"] -> case method of
                                "GET" -> ifValidRequest
                                    "getArticlesSortedByAuthor"
                                    maybeOffsetRequest
                                _ -> "Method is not implemented"
                            ["sortByCategory"] -> case method of
                                "GET" -> ifValidRequest
                                    "getArticlesSortedByCategory"
                                    maybeOffsetRequest
                                _ -> "Method is not implemented"
                            ["createdAt"] -> case method of
                                "GET" -> ifValidRequest
                                    "getArticlesFilteredByCreationDate"
                                    maybeArticlesFilteredByCreationDate
                                _ -> "Method is not implemented"
                            ["createdBefore"] -> case method of
                                "GET" -> ifValidRequest
                                    "getArticlesCreatedBeforeDate"
                                    maybeArticlesFilteredByCreationDate
                                _ -> "Method is not implemented"
                            ["createdAfter"] -> case method of
                                "GET" -> ifValidRequest
                                    "getArticlesCreatedAfterDate"
                                    maybeArticlesFilteredByCreationDate
                                _ -> "Method is not implemented"
                            _ -> "No such endpoint"
                        _ -> "No such endpoint")
                    else "Endpoint needed")

            results <- let {
                runSession session = (session . fromJust $ decode requestBody);
                sessionAuthorId = (read sessionAuthorIdString :: Int32);
                sessionResults = case errorOrSessionName of
                    "auth" -> runSession HSS.getCredentials
                        >>= (\ (eitherSessionResult, errorForClient) ->
                            (processCredentials eitherSessionResult
                            >>= (\ processedCreds -> pure (processedCreds, errorForClient))
                            )
                        )
                    "createUser" -> runSession HSS.createUser
                    "getUser" -> HSS.getUser (read sessionUserId :: Int32)
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
                    "createComment" -> runSession HSS.createComment sessionAuthorId
                    "deleteComment" -> runSession HSS.deleteComment sessionAuthorId
                    --"getArticleComments" -> runSession HSS.getArticleComments
                    --"createArticleDraft" -> runSession HSS.createArticleDraft sessionAuthorId
                    --"editArticleDraft" -> runSession HSS.editArticleDraft sessionAuthorId
                    --"publishArticleDraft" -> runSession HSS.publishArticleDraft sessionAuthorId
                    --"getArticleDraft" -> runSession HSS.getArticleDraft sessionAuthorId
                    --"deleteArticleDraft" -> runSession HSS.deleteArticleDraft sessionAuthorId
                    --"getArticlesByCategoryId" -> runSession HSS.getArticlesByCategoryId
                    --"getArticlesByTagId" -> runSession HSS.getArticlesByTagId
                    --"getArticlesByAnyTagId" -> runSession HSS.getArticlesByAnyTagId
                    --"getArticlesByAllTagId" -> runSession HSS.getArticlesByAllTagId
                    --"getArticlesByTitlePart" -> runSession HSS.getArticlesByTitlePart
                    --"getArticlesByContentPart" -> runSession HSS.getArticlesByContentPart
                    --"getArticlesByAuthorNamePart" -> runSession HSS.getArticlesByAuthorNamePart
                    --"getArticlesSortedByPhotosNumber" -> runSession HSS.getArticlesSortedByPhotosNumber
                    --"getArticlesSortedByCreationDate" -> runSession HSS.getArticlesSortedByCreationDate
                    --"getArticlesSortedByAuthor" -> runSession HSS.getArticlesSortedByAuthor
                    --"getArticlesSortedByCategory" -> runSession HSS.getArticlesSortedByCategory
                    --"getArticlesFilteredByCreationDate" -> runSession HSS.getArticlesFilteredByCreationDate
                    --"getArticlesCreatedBeforeDate" -> runSession HSS.getArticlesCreatedBeforeDate
                    --"getArticlesCreatedAfterDate" -> runSession HSS.getArticlesCreatedAfterDate
                    nonMatched -> pure (Right $ UTFLBS.fromString nonMatched, Just $ UTFLBS.fromString nonMatched);
                } in sessionResults

            debugM "rest-news" (case fst results of
                Right ulbs -> UTFLBS.toString ulbs
                leftErr -> show (snd results, leftErr)
                )

            processedResults <- pure (case fst results of
                Right ulbs -> ulbs
                _ -> case snd results of
                    Just errorForClient -> errorForClient
                    -- there must not be any QueryError!
                    _ -> undefined)
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
    void (
        let {
            clearSessionPartial = clearSession simpleConnection "SESSION";
        } in run port
        -- :: SessionStore m k v	The SessionStore to use for sessions
        -- -> ByteString	        Name to use for the session cookie (MUST BE ASCII)
        -- -> SetCookie	            Settings for the cookie (path, expiry, etc)
        -- -> Key (Session m k v)	Vault key to use when passing the session through
        -- -> Middleware	 
        $ withSession store "SESSION" defaultSetCookie vaultK
        $ restAPI vaultK clearSessionPartial)

runWarpStatic :: IO ()
runWarpStatic = let {
    port = 8080 :: Port;
} in do
    run port $ staticApp (defaultWebAppSettings "static")

runWarpWithLogger :: IO ()
runWarpWithLogger = traplogging "rest-news" ERROR "shutdown due to"
    $ updateGlobalLogger "rest-news" (setLevel DEBUG)
    >> runWarp
