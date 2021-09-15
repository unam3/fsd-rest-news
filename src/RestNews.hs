{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings  #-}

module RestNews
    ( SessionErrorThatNeverOccured(..)
    , makeApplication
    , runWarpWithLogger
    , restAPI
    , processConfig
    ) where

import qualified RestNews.Config as C
import qualified RestNews.DBConnection as DBC
import qualified RestNews.DB.ProcessRequest as PR
import RestNews.DB.RequestRunner (cantDecode, cantDecodeBS, cantDecodeS, runSession)
import qualified RestNews.Logger as L
import qualified RestNews.Middleware.Sessions as S
import qualified RestNews.Requests.PrerequisitesCheck as PC
import RestNews.Requests.SessionName (getSessionName)
import qualified RestNews.Middleware.Static as Static
import qualified RestNews.WAI as WAI

import qualified Data.ByteString.Lazy.UTF8 as UTFLBS
import Control.Exception (Exception, bracket_, throw)
import Control.Monad (void, when)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Either (fromRight, isLeft)
import Data.Int (Int32)
import Data.Maybe (fromMaybe, isJust, isNothing)
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
import System.Log.Logger (Priority (DEBUG, ERROR), debugM, infoM, errorM, setLevel, traplogging, updateGlobalLogger)
import Web.Cookie (defaultSetCookie)

dbError :: UTFLBS.ByteString
dbError = "DB connection error"

getIdString :: Maybe String -> String
getIdString = fromMaybe "0"

getSessionName' :: 
    L.Handle a
    -> WAI.Handle a
    -> Request
    -> IO (Either String String)
getSessionName' loggerH waiH request = do
    _ <- liftIO . L.hDebug loggerH $ show request

    let method = WAI.hRequestMethod waiH request
        pathTextChunks = WAI.hPathInfo waiH request

    _ <- liftIO . L.hInfo loggerH $ show (method, pathTextChunks)

    let eitherSessionName = getSessionName (pathTextChunks, method)

    _ <- liftIO . L.hInfo loggerH $ show eitherSessionName

    pure eitherSessionName


data DBSessionNameAndSessionThings = DBSessionNameAndSessionThings {
    sessionName :: String,
    maybeUserId :: Maybe String,
    sessionUserIdString :: String,
    sessionAuthorIdString :: String,
    sessionLookup :: String -> IO (Maybe String),
    sessionInsert :: String -> String -> IO (),
    clearSessionPartial :: Request -> IO ()
}

data SessionErrorThatNeverOccured = SessionErrorThatNeverOccured deriving Show

instance Exception SessionErrorThatNeverOccured

prerequisitesCheck :: 
    L.Handle a
    -> S.Handle
    -> Request
    -> String
    -> IO (Either String DBSessionNameAndSessionThings)
prerequisitesCheck loggerH sessionsH request sessionName' = do
    let maybeSessionMethods = S.hMaybeSessionMethods sessionsH request
        (sessionLookup', sessionInsert') = fromMaybe (throw SessionErrorThatNeverOccured) maybeSessionMethods

    maybeUserId' <- sessionLookup' "user_id"
    maybeIsAdmin <- sessionLookup' "is_admin"
    maybeAuthorId <- sessionLookup' "author_id"

    _ <- liftIO . L.hDebug loggerH $ show ("session user_id" :: String, maybeUserId')
    _ <- liftIO . L.hDebug loggerH $ show ("session is_admin" :: String, maybeIsAdmin)
    _ <- liftIO . L.hDebug loggerH $ show ("session author_id" :: String, maybeAuthorId)

    let sessionUserIdString' = getIdString maybeUserId'
        sessionAuthorIdString' = getIdString maybeAuthorId
        params = PC.Params {
            PC.isAdmin = maybeIsAdmin == Just "True",
            PC.hasUserId = sessionUserIdString' /= "0",
            PC.hasAuthorId = sessionAuthorIdString' /= "0"
        }
    
    pure . fmap (\sessionNameFromRight -> DBSessionNameAndSessionThings
                    sessionNameFromRight
                    maybeUserId'
                    sessionUserIdString'
                    sessionAuthorIdString'
                    sessionLookup'
                    sessionInsert'
                    (S.hClearSession sessionsH)) $ PC.prerequisitesCheck params sessionName'


processCredentials :: Monad m => (String -> m a)
    -> (Request -> m ())
    -> Request
    -> Maybe String
    -> (String -> String -> m ())
    -> PR.HasqlSessionResults (Int32, Bool, Int32)
    -> m (PR.HasqlSessionResults (Int32, Bool, Int32))
processCredentials
    debug
    clearSessionPartial'
    request
    maybeUserId'
    sessionInsert'
    wrappedSessionResults = do
        let (PR.H sessionResults) = wrappedSessionResults
            (user_id, is_admin, author_id) = fromRight (0, False, 0) sessionResults
        -- clearSession will fail if request has no associated session with cookies:
        -- https://github.com/hce/postgresql-session/blob/master/src/Network/Wai/Session/PostgreSQL.hs#L232
        (do
            when
                (isJust maybeUserId')
                (clearSessionPartial' request)
            )
        _ <- debug (show ("put into sessions:" :: String, user_id, is_admin, author_id))
        sessionInsert' "is_admin" (show is_admin)
        sessionInsert' "user_id" (show user_id)
        sessionInsert' "author_id" (show author_id)
        pure wrappedSessionResults


newtype HasqlSessionError = HasqlSessionError String deriving Show

instance Exception HasqlSessionError

runDBSession ::
    L.Handle a
    -> WAI.Handle a
    -> DBC.Handle
    -> Request
    -> DBSessionNameAndSessionThings
    -> IO (Either String UTFLBS.ByteString)
runDBSession
    loggerH
    waiH
    dbH
    request
    (DBSessionNameAndSessionThings
        sessionName'
        maybeUserId'
        sessionUserIdString'
        sessionAuthorIdString'
        _
        sessionInsert'
        clearSessionPartial'
    ) = do
        let runSessionResults = do
                requestBody <- WAI.hStrictRequestBody waiH request

                _ <- liftIO . L.hInfo loggerH $ show requestBody

                eitherConnection <- DBC.hAcquiredConnection dbH

                case eitherConnection of
                    Left connectionError -> 
                        liftIO $ L.hError loggerH (show connectionError)
                            >> pure (PR.H $ Left $ Right dbError)
                    Right connection ->
                        let processCredentialsPartial =
                                processCredentials
                                    (L.hDebug loggerH)
                                    clearSessionPartial'
                                    request
                                    maybeUserId'
                                    sessionInsert'
                            sessionAuthorId' = (read sessionAuthorIdString' :: Int32)
                            sessionUserId' = (read sessionUserIdString' :: Int32)
                        in runSession
                            connection
                            requestBody
                            processCredentialsPartial
                            sessionUserId'
                            sessionAuthorId'
                            sessionName'

        hRunSessionResults <- runSessionResults

        _ <- liftIO . L.hDebug loggerH $ show hRunSessionResults

        let (PR.H runSessionResultsUnpacked) = hRunSessionResults

        pure $ case runSessionResultsUnpacked of
            Left (Left unhandledError) -> throw $ HasqlSessionError unhandledError
            Left (Right errorForUser) -> Left $ UTFLBS.toString errorForUser
            Right runSessionResults' -> Right runSessionResults'

-- newtype ExceptT e m a = ExceptT (m (Either e a))
--respond' :: (Response -> IO ResponseReceived) -> ExceptT String IO String -> IO ResponseReceived
--respond' respond exceptT = runExceptT exceptT >>= (\case
--    Left error -> respond $ responseLBS H.status400 [] $ UTFLBS.fromString error
--    Right results -> respond $ responseLBS H.status200 [] $ UTFLBS.fromString results)


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
            let exceptTSessionName =
                    ExceptT (getSessionName' loggerH waiH request)
                        >>= (ExceptT . prerequisitesCheck loggerH sessionsH request)
                            >>= (ExceptT . runDBSession loggerH waiH dbH request)
                
                -- >>= respond' respond

            runExceptT exceptTSessionName >>= (\case
                Left error ->
                    let status =
                            if error == cantDecodeS
                            then H.status400
                            else H.status404
                    in respond $ responseLBS status [] $ UTFLBS.fromString error
                Right results -> respond $ responseLBS H.status200 [] results)
            )

            --processedResults <-
            --    let no_output_for_the_user_in_case_of_unhandled_exception = ""
            --    in (case fst results of
            --        Right ulbs -> pure ulbs
            --        _ -> case snd results of
            --            Just errorForClient -> pure errorForClient
            --            _ -> L.hError
            --                    loggerH
            --                    "\n^^^ unhandled exception has occured with request above^^^\n\n"
            --                        >> pure no_output_for_the_user_in_case_of_unhandled_exception)

            --let has_unhandled_hasql_session_exception = isLeft (fst results) && isNothing (snd results)
            --    httpStatus
            --        | errorOrSessionName == PC.noSuchEndpoint = H.status404
            --        | results == cantDecode = H.status400
            --        | fst results == dbError
            --            || has_unhandled_hasql_session_exception = H.status500
            --        | otherwise = H.status200
            --    in respond $ responseLBS httpStatus [] processedResults)


processConfig :: C.Config -> (Port, Settings, ConnectInfo)
processConfig (C.Config runAtPort dbHost dbPort dbUser dbPassword dbName) =
    (
        runAtPort,
        settings (fromString dbHost) (toEnum dbPort) (fromString dbUser) (fromString dbPassword) (fromString dbName),
        ConnectInfo {
            connectHost = dbHost,
            connectPort = toEnum dbPort,
            connectUser = dbUser,
            connectPassword = dbPassword,
            connectDatabase = dbName
        }
    )


makeApplication :: L.Handle () -> Settings -> ConnectInfo -> IO Application
makeApplication loggerH dbConnectionSettings connectInfo =  
    do  
        let storeSettings = defaultSettings {storeSettingsLog = L.hDebug loggerH}
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
                        WAI.withWAI
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
            

runWarpWithLogger :: IO ()
runWarpWithLogger =
    do
        L.withLogger
            (L.Config
                -- use INFO, DEBUG or ERROR here
                -- (add to System.Log.Logger import items if missed)
                DEBUG
                (traplogging
                    "rest-news"
                    ERROR
                    "Unhandled exception occured"
                    . updateGlobalLogger "rest-news" . setLevel)
                (debugM "rest-news")
                (infoM "rest-news")
                (errorM "rest-news"))
            (\ loggerH ->
                C.parseConfig "config.ini"
                    >>= \ case
                        Left errorMessage ->
                            L.hError loggerH errorMessage
                                >> exitFailure
                        Right config ->
                            let (port, dbConnectionSettings, connectInfo) = processConfig config
                            in makeApplication loggerH dbConnectionSettings connectInfo
                                >>= run port
                )

        pure ()
