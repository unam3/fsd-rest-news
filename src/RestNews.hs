{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module RestNews
    ( runWarpWithLogger
    ) where

import Control.Exception (bracket_)
import Control.Monad (void)
import Data.Int (Int64)
import qualified Hasql.Connection as Connection
--import qualified Hasql.Decoders as Decoders
--import qualified Hasql.Encoders as Encoders
import Hasql.Session (Session)
import Hasql.Statement (Statement(..))
import qualified Hasql.Session as Session
import qualified Hasql.TH as TH
import qualified Network.HTTP.Types as H
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (Port, run)
import System.Log.Logger (Priority (DEBUG, ERROR), debugM, setLevel, traplogging, updateGlobalLogger)

-- https://hackage.haskell.org/package/hasql-1.4.4
-- https://github.com/nikita-volkov/hasql-tutorial1

selectTest :: Session (Int64)
selectTest = Session.statement ()
    [TH.singletonStatement|
        select ((2 :: int8) + (2 :: int8)) :: int8
        |]

dbCall :: IO ()
dbCall = let {
    connectionSettings = Connection.settings "localhost" 5432 "" "" "yay";
} in do
    connectionAcquired <- Connection.acquire connectionSettings
    void . print $ case connectionAcquired of
        Right connection -> Session.run selectTest connection
        Left connectionError -> connectionError

restAPI :: Application;
restAPI request respond = bracket_
    (debugM "rest-news" "Allocating scarce resource")
    (debugM "rest-news" "Cleaning up")
    (respond $ responseLBS H.status200 [] "Hello World")

runWarp :: IO ()
runWarp = let {
    port = 8081 :: Port;
} in run port restAPI

runWarpWithLogger :: IO ()
runWarpWithLogger = dbCall
