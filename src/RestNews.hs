{-# LANGUAGE OverloadedStrings  #-}

module RestNews
    ( runWarpWithLogger,
    processArgs
    ) where

import AesonDefinitions (AuthRequest, CreateUserRequest, UserIdRequest, PromoteUserToAuthorRequest, EditAuthorRequest, AuthorIdRequest, CreateCategoryRequest, UpdateCategoryRequest, CategoryIdRequest, CreateTagRequest, EditTagRequest, TagIdRequest, TagIdRequestWithOffset, CreateCommentRequest, CreateCommentRequest, CommentIdRequest, ArticleCommentsRequest, ArticleDraftRequest, ArticleDraftEditRequest, ArticleDraftIdRequest, ArticlesByCategoryIdRequest, ArticlesByTagIdListRequest, ArticlesByTitlePartRequest, ArticlesByContentPartRequest, ArticlesByAuthorNamePartRequest, ArticlesByCreationDateRequest, OffsetRequest)
import qualified HasqlSessions as HSS

import Control.Exception (bracket_)
import Control.Monad (void, when)
import Data.Aeson (decode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as UTFLBS
import Data.Either (fromRight)
import Data.Int (Int32)
import qualified Data.HashMap.Strict as HMS
import Data.Maybe (fromJust, isJust)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Vault.Lazy as Vault
import Database.PostgreSQL.Simple (Connection, ConnectInfo(..), connectPostgreSQL, postgreSQLConnectionString)
import Hasql.Connection (Settings, settings)
import qualified Network.HTTP.Types as H
import Network.Wai (Application, Request, pathInfo, requestMethod, responseLBS, strictRequestBody, vault)
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp (Port, run)
import Network.Wai.Middleware.Rewrite (PathsAndQueries, rewritePureWithQueries)
import Network.Wai.Session (withSession, Session)
import Prelude hiding (error)
import Network.Wai.Session.PostgreSQL (clearSession, dbStore, defaultSettings, fromSimpleConnection, purger)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.Log.Logger (Priority (DEBUG, ERROR), debugM, errorM, setLevel, traplogging, updateGlobalLogger)
import Web.Cookie (defaultSetCookie)

wrongParamsOrValues :: Either String String
wrongParamsOrValues = Left "Wrong parameters/parameters values"

passSessionNameIfValidRequest :: String -> Maybe request -> Either String String
passSessionNameIfValidRequest sessionName request = if sessionName == noSuchEndpointS
    then noSuchEndpoint
    else maybe wrongParamsOrValues (const $ Right sessionName) request

noSuchEndpointS :: String
noSuchEndpointS = "No such endpoint"

noSuchEndpoint :: Either String String
noSuchEndpoint = Left "No such endpoint"

passSessionNameIf :: String -> Bool -> Either String String
passSessionNameIf sessionName condition = if condition
    then Right sessionName
    else noSuchEndpoint;

dbError :: Either String UTFLBS.ByteString
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
restAPI :: Settings -> Vault.Key (Session IO String String) -> (Request -> IO ()) -> Application;
restAPI dbConnectionSettings vaultKey clearSessionPartial request respond = let {
        endpointNeeded = Left "Endpoint needed";
        pathTextChunks = pathInfo request;
        method = requestMethod request;
        Just (sessionLookup, sessionInsert) = Vault.lookup vaultKey (vault request);
        processCredentials :: Either String (Int32, Bool, Int32) -> IO (Either String UTFLBS.ByteString);
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

            >> (pure $ fmap (const "cookies are baked") sessionResults);
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
                maybeArticleCommentsRequestJSON = decode requestBody :: Maybe ArticleCommentsRequest;
                maybeArticleDraftRequestJSON = decode requestBody :: Maybe ArticleDraftRequest;
                maybeArticleDraftEditRequestJSON = decode requestBody :: Maybe ArticleDraftEditRequest;
                maybeArticlesByCategoryIdRequestJSON = decode requestBody :: Maybe ArticlesByCategoryIdRequest;
                maybeArticlesByTagIdListRequest = decode requestBody :: Maybe ArticlesByTagIdListRequest;
                maybeArticlesByTitlePartRequest = decode requestBody :: Maybe ArticlesByTitlePartRequest;
                maybeArticlesByContentPartRequest = decode requestBody :: Maybe ArticlesByContentPartRequest;
                maybeArticlesByAuthorNamePartRequest = decode requestBody :: Maybe ArticlesByAuthorNamePartRequest;
                maybeArticlesFilteredByCreationDate = decode requestBody :: Maybe ArticlesByCreationDateRequest;
                maybeOffsetRequest = decode requestBody :: Maybe OffsetRequest;
                pathAndMethodToF :: HMS.HashMap ([Text], BS.ByteString) (LBS.ByteString -> Either String String);
                pathAndMethodToF = HMS.fromList [
                    --( 
                    --    (pathTextChunks, method),
                    --    (requestBody) -> sessionNameOrError)
                    ----- (isAdmin, sessionUserIdString, sessionAuthorIdString, requestBody) -> sessionNameOrError)
                    --)
                    ((["auth"], "POST"),
                        passSessionNameIfValidRequest
                            "auth"
                            . (decode :: LBS.ByteString -> Maybe AuthRequest)
                            ),
                    ((["users"], "POST"),
                        passSessionNameIfValidRequest
                            "createUser"
                            . (decode :: LBS.ByteString -> Maybe CreateUserRequest)
                            ),
                    ((["users"], "GET"),
                        passSessionNameIfHasUserId
                            "getUser"
                        ),
                    ((["users"], "DELETE"),
                        passSessionNameIfValidRequest
                            passSessionNameIfAdmin "deleteUser"
                            . (decode :: LBS.ByteString -> Maybe UserIdRequest)
                            ),
                    ((["authors"], "POST"),
                        passSessionNameIfValidRequest
                            passSessionNameIfAdmin "promoteUserToAuthor"
                            . (decode :: LBS.ByteString -> Maybe PromoteUserToAuthorRequest)
                            ),
--                    ((["authors"], "PATCH",
--                        passSessionNameIfValidRequest maybeEditAuthorRequestJSON
--                            $ passSessionNameIfAdmin "editAuthor"),
--                    ((["authors"], "GET"),
--                        passSessionNameIfValidRequest maybeAuthorIdRequestJSON
--                            $ passSessionNameIfAdmin "getAuthor"),
--                    ((["authors"], "DELETE"),
--                        passSessionNameIfValidRequest maybeAuthorIdRequestJSON
--                            $ passSessionNameIfAdmin "deleteAuthorRole"),
--                    ((["categories"], "POST"),
--                        passSessionNameIfValidRequest maybeCreateCategoryRequestJSON
--                            $ passSessionNameIfAdmin "createCategory"),
--                    ((["categories"], "PATCH"),
--                        passSessionNameIfValidRequest maybeUpdateCategoryRequestJSON
--                            $ passSessionNameIfAdmin "updateCategory"),
--                    ((["categories"], "GET"),
--                        passSessionNameIfValidRequest maybeCategoryIdRequestJSON "getCategory")
--                    ((["categories"], "DELETE"),
--                        passSessionNameIfValidRequest maybeCategoryIdRequestJSON
--                            $ passSessionNameIfAdmin "deleteCategory"),
--                    ((["tags"], "POST"),
--                        passSessionNameIfValidRequest maybeCreateTagRequestJSON
--                            $ passSessionNameIfAdmin "createTag"),
--                    ((["tags"], "PATCH"),
--                        passSessionNameIfValidRequest maybeEditTagRequestJSON
--                            $ passSessionNameIfAdmin "editTag"),
--                    ((["tags"], "GET"),
--                        passSessionNameIfValidRequest maybeTagIdRequestJSON "getTag"),
--                    ((["tags"], "DELETE"),
--                        passSessionNameIfValidRequest maybeTagIdRequestJSON
--                            $ passSessionNameIfAdmin "deleteTag"),
--                    ((["comments"], "POST"),
--                        passSessionNameIfValidRequest maybeCreateCommentRequestJSON
--                            . either id id $ passSessionNameIfHasUserId "createComment"),
--                    ((["comments"], "GET"),
--                        passSessionNameIfValidRequest maybeArticleCommentsRequestJSON "getArticleComments"),
                    ((["comments"], "DELETE"),
                        passSessionNameIfValidRequest
                            either id id $ passSessionNameIfHasUserId "deleteComment"
                            . (decode :: LBS.ByteString -> Maybe CommentIdRequest)
                            )
                    --((["articles"], "POST"),
                    --    if isJust maybeArticleDraftRequestJSON
                    --        then passSessionNameIfHasAuthorId "createArticleDraft"
                    --        else passSessionNameIfValidRequest
                    --            (decode requestBody :: Maybe ArticleDraftIdRequest)
                    --            . either id id $ passSessionNameIfHasAuthorId "publishArticleDraft"),
                    --((["articles"], "PATCH"),
                    --    passSessionNameIfValidRequest maybeArticleDraftEditRequestJSON
                    --        . either id id $ passSessionNameIfHasAuthorId "editArticleDraft"),
                    --((["articles"], "GET"),
                    --    passSessionNameIfValidRequest
                    --        (decode requestBody :: Maybe ArticleDraftIdRequest)
                    --        . either id id $ passSessionNameIfHasAuthorId "getArticleDraft"),
                    --((["articles"], "DELETE"),
                    --    passSessionNameIfValidRequest
                    --        (decode requestBody :: Maybe ArticleDraftIdRequest)
                    --        . either id id $ passSessionNameIfHasAuthorId "deleteArticleDraft"),
                    --((["articles", "category"], "GET"),
                    --    passSessionNameIfValidRequest
                    --        maybeArticlesByCategoryIdRequestJSON
                    --        "getArticlesByCategoryId"),
                    --((["articles", "tag"], "GET"),
                    --    passSessionNameIfValidRequest maybeTagIdRequestWithOffsetJSON "getArticlesByTagId"),
                    --((["articles", "tags__any"], "GET"),
                    --    passSessionNameIfValidRequest maybeArticlesByTagIdListRequest "getArticlesByAnyTagId"),
                    --((["articles", "tags__all"], "GET"),
                    --    passSessionNameIfValidRequest maybeArticlesByTagIdListRequest "getArticlesByAllTagId"),
                    --((["articles", "in__title"], "GET"),
                    --    passSessionNameIfValidRequest maybeArticlesByTitlePartRequest "getArticlesByTitlePart"),
                    --((["articles", "in__content"], "GET"),
                    --    passSessionNameIfValidRequest maybeArticlesByContentPartRequest "getArticlesByContentPart")
                    --((["articles", "in__author_name"], "GET"),
                    --    passSessionNameIfValidRequest
                    --        maybeArticlesByAuthorNamePartRequest
                    --        "getArticlesByAuthorNamePart"),
                    --((["articles", "byPhotosNumber"], "GET"),
                    --    passSessionNameIfValidRequest
                    --        maybeOffsetRequest
                    --        "getArticlesSortedByPhotosNumber"),
                    --((["articles", "byCreationDate"], "GET"),
                    --    passSessionNameIfValidRequest
                    --        maybeOffsetRequest
                    --        "getArticlesSortedByCreationDate"),
                    --((["articles", "sortByAuthor"], "GET"),
                    --    passSessionNameIfValidRequest
                    --        maybeOffsetRequest
                    --        "getArticlesSortedByAuthor"),
                    --((["articles", "sortByCategory"], "GET"),
                    --    passSessionNameIfValidRequest
                    --        maybeOffsetRequest
                    --        "getArticlesSortedByCategory"),
                    --((["articles", "createdAt"], "GET"),
                    --    passSessionNameIfValidRequest
                    --        maybeArticlesFilteredByCreationDate
                    --        "getArticlesFilteredByCreationDate"),
                    --((["articles", "createdBefore"], "GET"),
                    --    passSessionNameIfValidRequest
                    --        maybeArticlesFilteredByCreationDate
                    --        "getArticlesCreatedBeforeDate"),
                    --((["articles", "createdAfter"], "GET"), passSessionNameIfValidRequest
                    --        maybeArticlesFilteredByCreationDate
                    --        "getArticlesCreatedAfterDate")
                    ];
            } in pure (
                case HMS.lookup (pathTextChunks, requestBody) of
                    --    (isAdmin, sessionUserIdString, sessionAuthorIdString, requestBody) -> sessionNameOrError)
                    Just checkRequest -> checkRequest requestBody 
                    Nothing -> noSuchEndpoint
                )

            eitherConnection <- HSS.getConnection dbConnectionSettings

            results <- let {
                sessionAuthorId = (read sessionAuthorIdString :: Int32);
                sessionUserId = (read sessionUserIdString :: Int32);
                sessionResults = case errorOrSessionName of
                    Left error -> pure (Left error, Just $ UTFLBS.fromString error)
                    Right sessionName -> 
                        case eitherConnection of
                            Left connectionError -> 
                                errorM "rest-news" (show connectionError)
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
                                "createComment" -> runSession HSS.createComment sessionUserId
                                "deleteComment" -> runSession HSS.deleteComment sessionUserId
                                "getArticleComments" -> runSession HSS.getArticleComments
                                "createArticleDraft" -> runSession HSS.createArticleDraft sessionAuthorId
                                "editArticleDraft" -> runSession HSS.editArticleDraft sessionAuthorId
                                "publishArticleDraft" -> runSession HSS.publishArticleDraft sessionAuthorId
                                "getArticleDraft" -> runSession HSS.getArticleDraft sessionAuthorId
                                "deleteArticleDraft" -> runSession HSS.deleteArticleDraft sessionAuthorId
                                "getArticlesByCategoryId" -> runSession HSS.getArticlesByCategoryId
                                "getArticlesByTagId" -> runSession HSS.getArticlesByTagId
                                "getArticlesByAnyTagId" -> runSession HSS.getArticlesByAnyTagId
                                "getArticlesByAllTagId" -> runSession HSS.getArticlesByAllTagId
                                "getArticlesByTitlePart" -> runSession HSS.getArticlesByTitlePart
                                "getArticlesByContentPart" -> runSession HSS.getArticlesByContentPart
                                "getArticlesByAuthorNamePart" -> runSession HSS.getArticlesByAuthorNamePart
                                "getArticlesSortedByPhotosNumber" -> runSession HSS.getArticlesSortedByPhotosNumber
                                "getArticlesSortedByCreationDate" -> runSession HSS.getArticlesSortedByCreationDate
                                "getArticlesSortedByAuthor" -> runSession HSS.getArticlesSortedByAuthor
                                "getArticlesSortedByCategory" -> runSession HSS.getArticlesSortedByCategory
                                "getArticlesFilteredByCreationDate" -> runSession HSS.getArticlesFilteredByCreationDate
                                "getArticlesCreatedBeforeDate" -> runSession HSS.getArticlesCreatedBeforeDate
                                "getArticlesCreatedAfterDate" -> runSession HSS.getArticlesCreatedAfterDate
                                nonMatched -> pure (
                                    Left nonMatched,
                                    Nothing);
                } in sessionResults

            debugM
                "rest-news"
                (case fst results of
                    Left leftErr -> show (snd results) ++ ", " ++ leftErr
                    Right ulbs -> UTFLBS.toString ulbs
                )

            processedResults <- let {
                no_output_for_the_user_in_case_of_unhandled_exception = "";
            } in pure (case fst results of
                Right ulbs -> ulbs
                _ -> case snd results of
                    Just errorForClient -> errorForClient
                    _ -> no_output_for_the_user_in_case_of_unhandled_exception)

            let {
                httpStatus
                    | errorOrSessionName == endpointNeeded || errorOrSessionName == noSuchEndpoint = H.status404
                    | errorOrSessionName == wrongParamsOrValues = H.status400
                    | fst results == dbError = H.status500
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

processArgs :: [String] -> Either String (Port, Settings)
-- settings :: ByteString -> Word16 -> ByteString -> ByteString -> ByteString -> Settings
-- connectionSettings = settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
processArgs [runAtPort, dbHost, dbPort, dbUser, dbPassword, dbName] =
    Right (
        read runAtPort,
        settings (fromString dbHost) (read dbPort) (fromString dbUser) (fromString dbPassword) (fromString dbName)
    )
        
processArgs _ = Left "Exactly 6 arguments needed: port to run rest-news, db hostname, db port, db user, db password, db name"

runWarp :: [String] -> IO ()
runWarp argsList = let {
    processedArgs = processArgs argsList;
} in case processedArgs of
    Left error -> errorM "rest-news" error  
        >> exitFailure
    Right (port, dbConnectionSettings) -> 
        do
        vaultK <- Vault.newKey
        simpleConnection <- dbconnect >>= fromSimpleConnection
        -- IO (SessionStore IO String String)
        store <- dbStore simpleConnection defaultSettings
        void (purger simpleConnection defaultSettings)
        let {
                clearSessionPartial = clearSession simpleConnection "SESSION";
            } in run port
                (router (
                    withSession store "SESSION" defaultSetCookie vaultK
                    $ restAPI dbConnectionSettings vaultK clearSessionPartial
                    )
                )
            >> exitSuccess

runWarpWithLogger :: IO ()
runWarpWithLogger = traplogging "rest-news" ERROR "shutdown due to"
    $ updateGlobalLogger "rest-news" (setLevel DEBUG)
    >> getArgs >>= runWarp
