{-# LANGUAGE OverloadedStrings  #-}

module RestNews
    ( runWarpWithLogger,
    processArgs
    ) where

import qualified RestNews.Logger as L
import RestNews.DB.RequestRunner (runSession)
import qualified RestNews.Requests.PrerequisitesCheck as PrerequisitesCheck
import qualified RestNews.Middleware.Static as Static

import Control.Exception (bracket_)
import Control.Monad (void, when)
import qualified Data.ByteString.Lazy.UTF8 as UTFLBS
import Data.Either (fromRight)
import Data.Int (Int32)
import qualified Data.HashMap.Strict as HMS
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Data.String (fromString)
import qualified Data.Vault.Lazy as Vault
import Database.PostgreSQL.Simple (ConnectInfo(..), connectPostgreSQL, postgreSQLConnectionString)
import Hasql.Connection (Settings, acquire, settings)
import qualified Network.HTTP.Types as H
import Network.Wai (Application, Request, pathInfo, requestMethod, responseLBS, strictRequestBody, vault)
import Network.Wai.Handler.Warp (Port, run)
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

processCredentials :: Monad m => (String -> m ())
    -> (String -> m (Maybe String))
    -> (Request -> m ())
    -> Request
    -> (String -> String -> m ())
    -> (Either String (Int32, Bool, Int32), Maybe UTFLBS.ByteString)
    -> m (Either String (Int32, Bool, Int32), Maybe UTFLBS.ByteString);
processCredentials debug sessionLookup clearSessionPartial request sessionInsert sessionResults = let {
    (user_id, is_admin, author_id) = fromRight (0, False, 0) $ fst sessionResults;
} in do
    -- clearSession will fail if request has no associated session with cookies:
    -- https://github.com/hce/postgresql-session/blob/master/src/Network/Wai/Session/PostgreSQL.hs#L232
    (do
        session_user_id <- sessionLookup "user_id"
        when
            (isJust session_user_id)
            (debug "invalidating session"
                >> clearSessionPartial request)
        )
    debug (show ("put into sessions:" :: String, user_id, is_admin, author_id))
    sessionInsert "is_admin" (show is_admin)
    sessionInsert "user_id" (show user_id)
    sessionInsert "author_id" (show author_id)
    pure sessionResults


--type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
restAPI :: L.Handle -> Settings -> Vault.Key (Session IO String String) -> (Request -> IO ()) -> Application;
restAPI loggerH dbConnectionSettings vaultKey clearSessionPartial request respond = let {
        pathTextChunks = pathInfo request;
        method = requestMethod request;
        sessionMethods = Vault.lookup vaultKey (vault request);
        (sessionLookup, sessionInsert) = fromJust sessionMethods;
    } in bracket_
        ((L.hDebug loggerH) "Allocating scarce resource")
        ((L.hDebug loggerH) "Cleaning up")
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

            (L.hDebug loggerH) $ show request
            (L.hDebug loggerH) $ show ("session user_id" :: String, maybeUserId)
            (L.hDebug loggerH) $ show ("session is_admin" :: String, maybeIsAdmin)
            (L.hDebug loggerH) $ show ("session author_id" :: String, maybeAuthorId)

            requestBody <- strictRequestBody request
            
            (L.hDebug loggerH) (show (method, pathTextChunks, requestBody))

            errorOrSessionName <- let {
                params = PrerequisitesCheck.Params {
                    PrerequisitesCheck.lbsRequest = requestBody,
                    PrerequisitesCheck.isAdmin = maybeIsAdmin == Just "True",
                    PrerequisitesCheck.hasUserId = sessionUserIdString /= "0",
                    PrerequisitesCheck.hasAuthorId = sessionAuthorIdString /= "0"
                    };
            } in pure (
                case HMS.lookup (pathTextChunks, method) PrerequisitesCheck.endpointToEitherSessionName of
                    Just checkRequest -> checkRequest params
                    Nothing -> PrerequisitesCheck.noSuchEndpoint
                )

            eitherConnection <- acquire dbConnectionSettings

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
                                    processCredentials (L.hDebug loggerH) sessionLookup clearSessionPartial request sessionInsert;
                                    sessionAuthorId = (read sessionAuthorIdString :: Int32);
                                    sessionUserId = (read sessionUserIdString :: Int32);
                            } in runSession
                                connection
                                requestBody
                                processCredentialsPartial
                                sessionUserId
                                sessionAuthorId
                                sessionName

            (L.hDebug loggerH)
                (case fst results of
                    Left leftErr -> show (snd results) ++ ", " ++ leftErr
                    Right ulbs -> UTFLBS.toString ulbs
                )

            processedResults <- let {
                no_output_for_the_user_in_case_of_unhandled_exception = "";
            } in (case fst results of
                Right ulbs -> pure ulbs
                _ -> case snd results of
                    Just errorForClient -> pure errorForClient
                    _ -> (L.hError loggerH) "\n^^^ unhandled exception ^^^\n\n"
                        >> pure no_output_for_the_user_in_case_of_unhandled_exception)

            let {
                endpointNeeded = Left "Endpoint needed";
                httpStatus
                    | errorOrSessionName == endpointNeeded
                        || errorOrSessionName == PrerequisitesCheck.noSuchEndpoint = H.status404
                    | errorOrSessionName == PrerequisitesCheck.wrongParamsOrValues = H.status400
                    | fst results == dbError = H.status500
                    | otherwise = H.status200;
            } in respond $ responseLBS httpStatus [] processedResults)


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


runWarp :: L.Handle -> [String] -> IO ()
runWarp loggerH argsList = let {
    processedArgs = processArgs argsList;
} in case processedArgs of
    Left error' -> (L.hError loggerH) error'
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
                (Static.router (
                    withSession store "SESSION" defaultSetCookie vaultK
                    $ restAPI loggerH dbConnectionSettings vaultK clearSessionPartial
                    )
                )
            >> exitSuccess

runWarpWithLogger :: IO ()
runWarpWithLogger = L.withLogger
    (L.Config
        DEBUG
        (\ priority -> traplogging
            "rest-news"
            ERROR
            "Unhandled exception occured"
            $ updateGlobalLogger "rest-news" (setLevel priority))
        (debugM "rest-news")
        (errorM "rest-news"))

    (\ loggerH -> getArgs >>= runWarp loggerH)
