{-# LANGUAGE OverloadedStrings  #-}

module RestNews
    ( makeApplication
    , runWarpWithLogger
    , restAPI
    , processArgs
    ) where

import qualified RestNews.DBConnection as DBC
import qualified RestNews.Logger as L
import qualified RestNews.Middleware.Sessions as S
import RestNews.DB.RequestRunner (runSession)
import qualified RestNews.Requests.PrerequisitesCheck as PrerequisitesCheck
import qualified RestNews.Middleware.Static as Static
import qualified RestNews.WAI as WAI

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
import Network.Wai.Session (SessionStore, withSession)
import Prelude hiding (error)
import Network.Wai.Session.PostgreSQL (clearSession, dbStore, defaultSettings, fromSimpleConnection, purger, storeSettingsLog)
import System.Exit (exitFailure)
import System.Log.Logger (Priority (DEBUG, ERROR), debugM, errorM, setLevel, traplogging, updateGlobalLogger)
import Web.Cookie (defaultSetCookie)

dbError :: Either String UTFLBS.ByteString
dbError = Left "DB error"

getIdString :: Maybe String -> String
getIdString = fromMaybe "0"

processCredentials :: Monad m => (String -> m a)
    -> (String -> m (Maybe String))
    -> (Request -> m ())
    -> Request
    -> (String -> String -> m ())
    -> (Either String (Int32, Bool, Int32), Maybe UTFLBS.ByteString)
    -> m (Either String (Int32, Bool, Int32), Maybe UTFLBS.ByteString);
processCredentials debug sessionLookup clearSessionPartial request sessionInsert sessionResults =
    let {
        (user_id, is_admin, author_id) = fromRight (0, False, 0) $ fst sessionResults;
    } in do
        -- clearSession will fail if request has no associated session with cookies:
        -- https://github.com/hce/postgresql-session/blob/master/src/Network/Wai/Session/PostgreSQL.hs#L232
        (do
            session_user_id <- sessionLookup "user_id"
            when
                (isJust session_user_id)
                (clearSessionPartial request)
            )
        _ <- debug (show ("put into sessions:" :: String, user_id, is_admin, author_id))
        sessionInsert "is_admin" (show is_admin)
        sessionInsert "user_id" (show user_id)
        sessionInsert "author_id" (show author_id)
        pure sessionResults

--type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
restAPI ::
    L.Handle a
    -> S.Handle
    -> DBC.Handle
    -> WAI.Handle a
    -> Application
restAPI loggerH sessionsH dbH waiH request respond =
    bracket_
        (L.hDebug loggerH "Allocating scarce resource")
        (L.hDebug loggerH "Cleaning up")
        (do
            let maybeSessionMethods = S.hMaybeSessionMethods sessionsH request

            when
                (isNothing maybeSessionMethods)
                (L.hError loggerH "Vault session error"
                    -- kills current warp child's thread
                    -- warp will return 500
                    >> exitFailure)

            let (sessionLookup, sessionInsert) = fromJust maybeSessionMethods

            maybeUserId <- sessionLookup "user_id"
            maybeIsAdmin <- sessionLookup "is_admin"
            maybeAuthorId <- sessionLookup "author_id"

            _ <- L.hDebug loggerH $ show request
            _ <- L.hDebug loggerH $ show ("session user_id" :: String, maybeUserId)
            _ <- L.hDebug loggerH $ show ("session is_admin" :: String, maybeIsAdmin)
            _ <- L.hDebug loggerH $ show ("session author_id" :: String, maybeAuthorId)


            let method = WAI.hRequestMethod waiH request
                pathTextChunks = WAI.hPathInfo waiH request

            requestBody <- WAI.hStrictRequestBody waiH request

            _ <- L.hDebug loggerH $ show (method, pathTextChunks, requestBody)

            let sessionUserIdString = getIdString maybeUserId
                sessionAuthorIdString = getIdString maybeAuthorId

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


            results <- case errorOrSessionName of
                Left error -> pure (Left error, Just $ UTFLBS.fromString error)
                Right sessionName -> 
                    do
                        eitherConnection <- DBC.hAcquiredConnection dbH

                        case eitherConnection of
                            Left connectionError -> 
                                L.hError loggerH (show connectionError)
                                >> pure (
                                    dbError,
                                    Just "DB connection error"
                                )
                            Right connection ->
                                let {
                                    processCredentialsPartial =
                                        processCredentials
                                            (L.hDebug loggerH)
                                            sessionLookup
                                            (S.hClearSession sessionsH)
                                            request
                                            sessionInsert;
                                    sessionAuthorId = (read sessionAuthorIdString :: Int32);
                                    sessionUserId = (read sessionUserIdString :: Int32);
                                } in runSession
                                    connection
                                    requestBody
                                    processCredentialsPartial
                                    sessionUserId
                                    sessionAuthorId
                                    sessionName

            _ <- L.hDebug
                loggerH
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
                    _ -> L.hError loggerH "\n^^^ unhandled exception ^^^\n\n"
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


makeApplication :: L.Handle () -> Settings -> ConnectInfo -> IO Application
makeApplication loggerH dbConnectionSettings connectInfo =  
        let storeSettings = defaultSettings {storeSettingsLog = L.hDebug loggerH}
        in do
            vaultKey <- Vault.newKey
            simpleConnection <- connectPostgreSQL (postgreSQLConnectionString connectInfo)
                >>= fromSimpleConnection
            store <- dbStore simpleConnection storeSettings :: IO (SessionStore IO String String)
            void (purger simpleConnection storeSettings)

            pure $ S.withSessions
                (S.Config
                    (withSession store "SESSION" defaultSetCookie vaultKey)
                    (Vault.lookup vaultKey . vault)
                    (clearSession simpleConnection "SESSION")
                )
                (\ sessionsH ->
                    DBC.withDBConnection
                        (DBC.Config $ acquire dbConnectionSettings)
                        (\ dbH ->
                            (WAI.withWAI
                                (WAI.Config
                                    requestMethod
                                    pathInfo
                                    strictRequestBody
                                )
                                (\ waiH ->
                                    Static.router (
                                        S.hWithSession
                                            sessionsH
                                            $ restAPI loggerH sessionsH dbH waiH
                                        )
                                )
                            )
                        )
                )
            

runWarpWithLogger :: [String] -> IO ()
runWarpWithLogger argsList =
    do
        L.withLogger
            (L.Config
                DEBUG
                (traplogging
                    "rest-news"
                    ERROR
                    "Unhandled exception occured"
                    . updateGlobalLogger "rest-news" . setLevel)
                (debugM "rest-news")
                (errorM "rest-news"))
            (\ loggerH ->
                let {
                    processedArgs = processArgs argsList;
                } in case processedArgs of
                    Left error' ->
                        L.hError loggerH error'
                            >> exitFailure
                    Right (port, dbConnectionSettings, connectInfo) ->
                        makeApplication loggerH dbConnectionSettings connectInfo >>= run port
                )

        pure ()
