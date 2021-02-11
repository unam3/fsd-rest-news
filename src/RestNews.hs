{-# LANGUAGE OverloadedStrings  #-}

module RestNews
    ( runWarpWithLogger
    ) where

import AesonDefinitions (AuthRequest, CreateUserRequest, UserIdRequest, PromoteUserToAuthorRequest, EditAuthorRequest, AuthorIdRequest, CreateCategoryRequest, UpdateCategoryRequest, CategoryIdRequest, CreateTagRequest, EditTagRequest, TagIdRequest, TagIdRequestWithOffset, CreateCommentRequest, CreateCommentRequest, CommentIdRequest, ArticleCommentsRequest, ArticleDraftRequest, ArticleDraftEditRequest, ArticleDraftIdRequest, ArticlesByCategoryIdRequest, ArticlesByTagIdListRequest, ArticlesByTitlePartRequest, ArticlesByContentPartRequest, ArticlesByAuthorNamePartRequest, ArticlesByCreationDateRequest, OffsetRequest)
import qualified HasqlSessions as HSS

import Control.Exception (bracket_)
import Control.Monad (void, when)
import Data.Aeson (encode, decode)
import Data.Bifunctor (bimap)
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
import Network.Wai.Middleware.Rewrite (PathsAndQueries, rewritePureWithQueries)
import Network.Wai.Session (withSession, Session)
import Prelude hiding (error)
import Network.Wai.Session.PostgreSQL (clearSession, dbStore, defaultSettings, fromSimpleConnection, purger)
import System.Log.Logger (Priority (DEBUG, ERROR), debugM, setLevel, traplogging, updateGlobalLogger)
import Web.Cookie (defaultSetCookie)

wrongParamsOrValues :: Either String String
wrongParamsOrValues = Left "Wrong parameters/parameters values"

ifValidRequest :: Maybe request -> String -> Either String String
ifValidRequest request sessionName = maybe wrongParamsOrValues (const $ Right sessionName) request

noSuchEndpointS :: String
noSuchEndpointS = "No such endpoint"

noSuchEndpoint :: Either String String
noSuchEndpoint = Left "No such endpoint"

passSessionNameIf :: String -> Bool -> Either String String
passSessionNameIf sessionName condition = if condition
    then Right sessionName
    else noSuchEndpoint;

dbError :: Either UTFLBS.ByteString UTFLBS.ByteString
dbError = Left "DB error"

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
        notImplemented = Left "Method is not implemented";
        endpointNeeded = Left "Endpoint needed";
        pathTextChunks = pathInfo request;
        isRequestPathNotEmpty = (not $ null pathTextChunks);
        pathHeadChunk = head pathTextChunks;
        method = requestMethod request;
        Just (sessionLookup, sessionInsert) = Vault.lookup vaultKey (vault request);
        processCredentials :: Either QueryError (Int32, Bool, Int32) -> IO (Either UTFLBS.ByteString UTFLBS.ByteString);
        processCredentials sessionResults = let {
            (user_id, is_admin, author_id) = fromRight (0, False, 0) sessionResults;
        } in
            -- clearSession will fail if request has no associated session with cookies:
            -- https://github.com/hce/postgresql-session/blob/master/src/Network/Wai/Session/PostgreSQL.hs#L232
            (do
                session_user_id <- sessionLookup "user_id"
                when
                    (isJust session_user_id)
                    (debugM "rest-news" "invalidating session"
                        >> clearSessionPartial request)
                )
            >> (debugM "rest-news" $ show ("put into sessions:" :: String, user_id, is_admin, author_id))
            >> (sessionInsert "is_admin" $ show is_admin)
            >> (sessionInsert "user_id" $ show user_id)
            >> (sessionInsert "author_id" $ show author_id)

            >> (pure $ bimap (encode . show) (const "cookies are baked") sessionResults);
    } in bracket_
        (debugM "rest-news" "Allocating scarce resource")
        (debugM "rest-news" "Cleaning up")
        (do
            maybeUserId <- sessionLookup "user_id"
            maybeIsAdmin <- sessionLookup "is_admin"
            maybeAuthorId <- sessionLookup "author_id"
            let sessionUserIdString = getIdString maybeUserId
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
                    else noSuchEndpointS;
                passSessionNameIfHasUserId sessionName = passSessionNameIf sessionName $ sessionUserIdString /= "0";
                passSessionNameIfHasAuthorId sessionName = passSessionNameIf sessionName $ sessionAuthorIdString /= "0";
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
                            "POST"      -> ifValidRequest maybeAuthRequestJSON "auth"
                            _ -> notImplemented
                        "users" -> case method of
                            "POST"      -> ifValidRequest maybeCreateUserRequestJSON "createUser"
                            "GET"       -> passSessionNameIfHasUserId "getUser"
                            "DELETE"    -> ifValidRequest maybeUserIdRequestJSON
                                $ passSessionNameIfAdmin "deleteUser" 
                            _ -> notImplemented
                        "authors" -> case method of
                            "POST"      -> ifValidRequest maybePromoteUserToAuthorRequestJSON
                                $ passSessionNameIfAdmin "promoteUserToAuthor"
                            "PATCH"     -> ifValidRequest maybeEditAuthorRequestJSON
                                $ passSessionNameIfAdmin "editAuthor"
                            "GET"       -> ifValidRequest maybeAuthorIdRequestJSON
                                $ passSessionNameIfAdmin "getAuthor" 
                            "DELETE"    -> ifValidRequest maybeAuthorIdRequestJSON
                                $ passSessionNameIfAdmin "deleteAuthorRole" 
                            _ -> notImplemented
                        "categories" -> case method of
                            "POST"      -> ifValidRequest maybeCreateCategoryRequestJSON
                                $ passSessionNameIfAdmin "createCategory"
                            "PATCH"     -> ifValidRequest maybeUpdateCategoryRequestJSON
                                $ passSessionNameIfAdmin "updateCategory"
                            "GET"       -> ifValidRequest maybeCategoryIdRequestJSON "getCategory"
                            "DELETE"    -> ifValidRequest maybeCategoryIdRequestJSON
                                $ passSessionNameIfAdmin "deleteCategory"
                            _ -> notImplemented
                        "tags" -> case method of
                            "POST"      -> ifValidRequest maybeCreateTagRequestJSON
                                $ passSessionNameIfAdmin "createTag"
                            "PATCH"     -> ifValidRequest maybeEditTagRequestJSON
                                $ passSessionNameIfAdmin "editTag"
                            "GET"       -> ifValidRequest maybeTagIdRequestJSON "getTag"
                            "DELETE"    -> ifValidRequest maybeTagIdRequestJSON
                                $ passSessionNameIfAdmin "deleteTag"
                            _ -> notImplemented
                        "comments" -> case method of
                            "POST"      -> ifValidRequest maybeCreateCommentRequestJSON
                                . either id id $ passSessionNameIfHasUserId "createComment"
                            "GET"       -> ifValidRequest maybeArticleCommentsRequestJSON "getArticleComments"
                            "DELETE"    -> ifValidRequest maybeCommentIdRequestJSON
                                . either id id $ passSessionNameIfHasUserId "deleteComment" 
                            _ -> notImplemented
                        "articles" -> case tail pathTextChunks of
                            [] -> case method of
                                "POST" -> if isJust maybeArticleDraftRequestJSON
                                    then passSessionNameIfHasAuthorId "createArticleDraft"
                                    else ifValidRequest maybeArticleDraftIdRequestJSON
                                        . either id id $ passSessionNameIfHasAuthorId "publishArticleDraft"
                                "PATCH" -> ifValidRequest maybeArticleDraftEditRequestJSON
                                    . either id id $ passSessionNameIfHasAuthorId "editArticleDraft"
                                "GET" -> ifValidRequest maybeArticleDraftIdRequestJSON
                                    . either id id $ passSessionNameIfHasAuthorId "getArticleDraft"
                                "DELETE" -> ifValidRequest maybeArticleDraftIdRequestJSON
                                    . either id id $ passSessionNameIfHasAuthorId "deleteArticleDraft"
                                _ -> notImplemented
                            ["category"] -> case method of
                                "GET" -> ifValidRequest maybeArticlesByCategoryIdRequestJSON "getArticlesByCategoryId" 
                                _ -> notImplemented
                            ["tag"] -> case method of
                                "GET" -> ifValidRequest maybeTagIdRequestWithOffsetJSON "getArticlesByTagId"
                                _ -> notImplemented
                            ["tags__any"] -> case method of
                                "GET" -> ifValidRequest maybeArticlesByTagIdListRequest "getArticlesByAnyTagId"
                                _ -> notImplemented
                            ["tags__all"] -> case method of
                                "GET" -> ifValidRequest maybeArticlesByTagIdListRequest "getArticlesByAllTagId"
                                _ -> notImplemented
                            ["in__title"] -> case method of
                                "GET" -> ifValidRequest maybeArticlesByTitlePartRequest "getArticlesByTitlePart"
                                _ -> notImplemented
                            ["in__content"] -> case method of
                                "GET" -> ifValidRequest maybeArticlesByContentPartRequest "getArticlesByContentPart"
                                _ -> notImplemented
                            ["in__author_name"] -> case method of
                                "GET" -> ifValidRequest
                                    maybeArticlesByAuthorNamePartRequest
                                    "getArticlesByAuthorNamePart"
                                _ -> notImplemented
                            ["byPhotosNumber"] -> case method of
                                "GET" -> ifValidRequest
                                    maybeOffsetRequest
                                    "getArticlesSortedByPhotosNumber"
                                _ -> notImplemented
                            ["byCreationDate"] -> case method of
                                "GET" -> ifValidRequest
                                    maybeOffsetRequest
                                    "getArticlesSortedByCreationDate"
                                _ -> notImplemented
                            ["sortByAuthor"] -> case method of
                                "GET" -> ifValidRequest
                                    maybeOffsetRequest
                                    "getArticlesSortedByAuthor"
                                _ -> notImplemented
                            ["sortByCategory"] -> case method of
                                "GET" -> ifValidRequest
                                    maybeOffsetRequest
                                    "getArticlesSortedByCategory"
                                _ -> notImplemented
                            ["createdAt"] -> case method of
                                "GET" -> ifValidRequest
                                    maybeArticlesFilteredByCreationDate
                                    "getArticlesFilteredByCreationDate"
                                _ -> notImplemented
                            ["createdBefore"] -> case method of
                                "GET" -> ifValidRequest
                                    maybeArticlesFilteredByCreationDate
                                    "getArticlesCreatedBeforeDate"
                                _ -> notImplemented
                            ["createdAfter"] -> case method of
                                "GET" -> ifValidRequest
                                    maybeArticlesFilteredByCreationDate
                                    "getArticlesCreatedAfterDate"
                                _ -> notImplemented
                            _ -> noSuchEndpoint
                        _ -> noSuchEndpoint)
                    else endpointNeeded)

            eitherConnection <- HSS.getConnection

            results <- let {
                sessionAuthorId = (read sessionAuthorIdString :: Int32);
                sessionUserId = (read sessionUserIdString :: Int32);
                sessionResults = case errorOrSessionName of
                    Left error -> pure (Right $ UTFLBS.fromString error, Just $ UTFLBS.fromString error)
                    Right sessionName -> 
                        case eitherConnection of
                            Left connectionError -> 
                                debugM "rest-news" (show connectionError)
                                >> pure (
                                    dbError,
                                    Just "DB connection error"
                                )
                            Right connection -> let {
                                runSession session = (session connection . fromJust $ decode requestBody);
                            } in case sessionName of
                                "auth" -> runSession HSS.getCredentials
                                    >>= (\ (eitherSessionResult, errorForClient) ->
                                        (processCredentials eitherSessionResult
                                        >>= (\ processedCreds -> pure (
                                            processedCreds,
                                            errorForClient
                                            ))
                                        )
                                    )
                                "createUser" -> runSession HSS.createUser;
                                "getUser" -> HSS.getUser connection sessionUserId
                                --"deleteUser" -> runSession HSS.deleteUser
                                --"promoteUserToAuthor" -> runSession HSS.promoteUserToAuthor;
                                --"editAuthor" -> runSession HSS.editAuthor
                                --"getAuthor" -> runSession HSS.getAuthor
                                --"deleteAuthorRole" -> runSession HSS.deleteAuthorRole
                                --"createCategory" -> runSession HSS.createCategory
                                --"updateCategory" -> runSession HSS.updateCategory
                                --"getCategory" -> runSession HSS.getCategory
                                --"deleteCategory" -> runSession HSS.deleteCategory
                                --"createTag" -> runSession HSS.createTag
                                --"editTag" -> runSession HSS.editTag
                                --"getTag" -> runSession HSS.getTag
                                --"deleteTag" -> runSession HSS.deleteTag
                                --"createComment" -> runSession HSS.createComment sessionUserId
                                --"deleteComment" -> runSession HSS.deleteComment sessionUserId
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
                                nonMatched -> pure (
                                    Right $ UTFLBS.fromString nonMatched,
                                    Just $ UTFLBS.fromString nonMatched);
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
                httpStatus
                    | errorOrSessionName == endpointNeeded || errorOrSessionName == noSuchEndpoint = H.status404
                    | errorOrSessionName == wrongParamsOrValues = H.status400
                    | errorOrSessionName == notImplemented = H.status501
                    | (fst results) == dbError = H.status500
                    | otherwise = H.status200;
            } in respond $ responseLBS httpStatus [] processedResults)

isRequestToStatic :: Request -> Bool
isRequestToStatic request =
    let {
        pathTextChunks = pathInfo request;
        isRequestPathNotEmpty = (not $ null pathTextChunks);
    } in isRequestPathNotEmpty && head pathTextChunks == "static"

removeStaticFromURI :: PathsAndQueries -> H.RequestHeaders -> PathsAndQueries
removeStaticFromURI ("static":otherPathPieces, queries) _ = (otherPathPieces, queries)
removeStaticFromURI pathsAndQueries _ = pathsAndQueries

rewrite :: Application -> Application
rewrite = rewritePureWithQueries removeStaticFromURI

router :: Application -> Application
router app request respond =
    if isRequestToStatic request
        then rewrite (staticApp (defaultWebAppSettings "static")) request respond
        else app request respond

runWarp :: IO ()
runWarp = let {
    port = 8081 :: Port;
} in do
    vaultK <- Vault.newKey
    simpleConnection <- dbconnect >>= fromSimpleConnection
    -- IO (SessionStore IO String String)
    store <- dbStore simpleConnection defaultSettings
    void (purger simpleConnection defaultSettings)
    void (
        let {
            clearSessionPartial = clearSession simpleConnection "SESSION";
        } in run port
        (router (
            withSession store "SESSION" defaultSetCookie vaultK
            $ restAPI vaultK clearSessionPartial
            ))
        )

runWarpWithLogger :: IO ()
runWarpWithLogger = traplogging "rest-news" ERROR "shutdown due to"
    $ updateGlobalLogger "rest-news" (setLevel DEBUG)
    >> runWarp
