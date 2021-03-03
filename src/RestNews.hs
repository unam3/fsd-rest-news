{-# LANGUAGE OverloadedStrings  #-}

module RestNews
    ( runWarpWithLogger,
    processArgs
    ) where

import qualified HasqlSessions as HSS
import qualified SessionPrerequisiteCheck as SessionPreCheck

import Control.Exception (bracket_)
import Control.Monad (void, when)
import Data.Aeson (decode)
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy.UTF8 as UTFLBS
import Data.Either (fromRight)
import Data.Int (Int32)
import qualified Data.HashMap.Strict as HMS
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Data.String (fromString)
import qualified Data.Vault.Lazy as Vault
import Database.PostgreSQL.Simple (ConnectInfo(..), connectPostgreSQL, postgreSQLConnectionString)
import Hasql.Connection (Connection, Settings, settings)
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

dbError :: Either String UTFLBS.ByteString
dbError = Left "DB error"

getIdString :: Maybe String -> String
getIdString = fromMaybe "0"

processCredentials :: (String -> IO (Maybe String))
    -> (Request -> IO ())
    -> Request
    -> (String -> String -> IO ())
    -> (Either String (Int32, Bool, Int32), Maybe UTFLBS.ByteString)
    -> IO (Either String (Int32, Bool, Int32), Maybe UTFLBS.ByteString);
processCredentials sessionLookup clearSessionPartial request sessionInsert sessionResults = let {
    (user_id, is_admin, author_id) = fromRight (0, False, 0) $ fst sessionResults;
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
    >> (debugM "rest-news"
        $ show ("put into sessions:" :: String, user_id, is_admin, author_id))
    >> (sessionInsert "is_admin" $ show is_admin)
    >> (sessionInsert "user_id" $ show user_id)
    >> (sessionInsert "author_id" $ show author_id)
    >> pure sessionResults

runSession ::
    Connection
    -> UTFLBS.ByteString
    -> ((Either String (Int32, Bool, Int32), Maybe UTFLBS.ByteString)
        -> IO (Either String (Int32, Bool, Int32), Maybe UTFLBS.ByteString))
    -> Int32
    -> Int32
    -> String
    -> IO (Either String UTFLBS.ByteString, Maybe UTFLBS.ByteString)
runSession
    connection
    requestBody
    processCredentialsPartial
    sessionUserId
    sessionAuthorId
    sessionName = let {
        runSessionWithJSON session = session connection . fromJust $ decode requestBody;
    } in case sessionName of
        "auth" -> runSessionWithJSON HSS.getCredentials
            >>= processCredentialsPartial
            >>= pure . first (fmap $ const "cookies are baked")
        "createUser" -> runSessionWithJSON HSS.createUser;
        "getUser" -> HSS.getUser connection sessionUserId
        "deleteUser" -> runSessionWithJSON HSS.deleteUser
        "promoteUserToAuthor" -> runSessionWithJSON HSS.promoteUserToAuthor;
        "editAuthor" -> runSessionWithJSON HSS.editAuthor
        "getAuthor" -> runSessionWithJSON HSS.getAuthor
        "deleteAuthorRole" -> runSessionWithJSON HSS.deleteAuthorRole
        "createCategory" -> runSessionWithJSON HSS.createCategory
        "updateCategory" -> runSessionWithJSON HSS.updateCategory
        "getCategory" -> runSessionWithJSON HSS.getCategory
        "deleteCategory" -> runSessionWithJSON HSS.deleteCategory
        "createTag" -> runSessionWithJSON HSS.createTag
        "editTag" -> runSessionWithJSON HSS.editTag
        "getTag" -> runSessionWithJSON HSS.getTag
        "deleteTag" -> runSessionWithJSON HSS.deleteTag
        "createComment" -> runSessionWithJSON HSS.createComment sessionUserId
        "deleteComment" -> runSessionWithJSON HSS.deleteComment sessionUserId
        "getArticleComments" -> runSessionWithJSON HSS.getArticleComments
        "createArticleDraft" -> runSessionWithJSON HSS.createArticleDraft sessionAuthorId
        "editArticleDraft" -> runSessionWithJSON HSS.editArticleDraft sessionAuthorId
        "publishArticleDraft" -> runSessionWithJSON HSS.publishArticleDraft sessionAuthorId
        "getArticleDraft" -> runSessionWithJSON HSS.getArticleDraft sessionAuthorId
        "deleteArticleDraft" -> runSessionWithJSON HSS.deleteArticleDraft sessionAuthorId
        "getArticlesByCategoryId" -> runSessionWithJSON HSS.getArticlesByCategoryId
        "getArticlesByTagId" -> runSessionWithJSON HSS.getArticlesByTagId
        "getArticlesByAnyTagId" -> runSessionWithJSON HSS.getArticlesByAnyTagId
        "getArticlesByAllTagId" -> runSessionWithJSON HSS.getArticlesByAllTagId
        "getArticlesByTitlePart" -> runSessionWithJSON HSS.getArticlesByTitlePart
        "getArticlesByContentPart" -> runSessionWithJSON HSS.getArticlesByContentPart
        "getArticlesByAuthorNamePart" -> runSessionWithJSON HSS.getArticlesByAuthorNamePart
        "getArticlesSortedByPhotosNumber" -> runSessionWithJSON HSS.getArticlesSortedByPhotosNumber
        "getArticlesSortedByCreationDate" -> runSessionWithJSON HSS.getArticlesSortedByCreationDate
        "getArticlesSortedByAuthor" -> runSessionWithJSON HSS.getArticlesSortedByAuthor
        "getArticlesSortedByCategory" -> runSessionWithJSON HSS.getArticlesSortedByCategory
        "getArticlesFilteredByCreationDate" -> runSessionWithJSON HSS.getArticlesFilteredByCreationDate
        "getArticlesCreatedBeforeDate" -> runSessionWithJSON HSS.getArticlesCreatedBeforeDate
        "getArticlesCreatedAfterDate" -> runSessionWithJSON HSS.getArticlesCreatedAfterDate
        nonMatched -> pure (
            Left nonMatched,
            Nothing
            )


--type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
restAPI :: Settings -> Vault.Key (Session IO String String) -> (Request -> IO ()) -> Application;
restAPI dbConnectionSettings vaultKey clearSessionPartial request respond = let {
        pathTextChunks = pathInfo request;
        method = requestMethod request;
        sessionMethods = Vault.lookup vaultKey (vault request);
        (sessionLookup, sessionInsert) = fromJust sessionMethods;
    } in bracket_
        (debugM "rest-news" "Allocating scarce resource")
        (debugM "rest-news" "Cleaning up")
        (do
            when
                (isNothing sessionMethods)
                (errorM "rest-news" "vault session error"
                    >> exitFailure)

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
                params = SessionPreCheck.Params {
                    SessionPreCheck.lbsRequest = requestBody,
                    SessionPreCheck.isAdmin = maybeIsAdmin == Just "True",
                    SessionPreCheck.hasUserId = sessionUserIdString /= "0",
                    SessionPreCheck.hasAuthorId = sessionAuthorIdString /= "0"
                    };
            } in pure (
                case HMS.lookup (pathTextChunks, method) SessionPreCheck.endpointToEitherSessionName of
                    Just checkRequest -> checkRequest params
                    Nothing -> SessionPreCheck.noSuchEndpoint
                )

            eitherConnection <- HSS.getConnection dbConnectionSettings

            results <- case errorOrSessionName of
                Left error -> pure (Left error, Just $ UTFLBS.fromString error)
                Right sessionName -> 
                    case eitherConnection of
                        Left connectionError -> 
                            errorM "rest-news" (show connectionError)
                            >> pure (
                                dbError,
                                Just "DB connection error"
                            )
                        Right connection ->
                            let {
                                processCredentialsPartial =
                                    processCredentials sessionLookup clearSessionPartial request sessionInsert;
                                    sessionAuthorId = (read sessionAuthorIdString :: Int32);
                                    sessionUserId = (read sessionUserIdString :: Int32);
                            } in runSession
                                connection
                                requestBody
                                processCredentialsPartial
                                sessionUserId
                                sessionAuthorId
                                sessionName

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
                endpointNeeded = Left "Endpoint needed";
                httpStatus
                    | errorOrSessionName == endpointNeeded
                        || errorOrSessionName == SessionPreCheck.noSuchEndpoint = H.status404
                    | errorOrSessionName == SessionPreCheck.wrongParamsOrValues = H.status400
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

processArgs :: [String] -> Either String (Port, Settings, ConnectInfo)
processArgs [runAtPort, dbHost, dbPort, dbUser, dbPassword, dbName] =
    Right (
        read runAtPort,
        settings (fromString dbHost) (read dbPort) (fromString dbUser) (fromString dbPassword) (fromString dbName),
        ConnectInfo {
            connectHost = dbHost,
            connectPort = read dbPort,
            connectUser = dbUser,
            connectPassword = dbPassword,
            connectDatabase = dbName
        }
    )
        
processArgs _ = Left "Exactly 6 arguments needed: port to run rest-news, db hostname, db port, db user, db password, db name"


runWarp :: [String] -> IO ()
runWarp argsList = let {
    processedArgs = processArgs argsList;
} in case processedArgs of
    Left error -> errorM "rest-news" error  
        >> exitFailure
    Right (port, dbConnectionSettings, connectInfo) -> 
        do
        vaultK <- Vault.newKey
        simpleConnection <- connectPostgreSQL (postgreSQLConnectionString connectInfo)
            >>= fromSimpleConnection
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
