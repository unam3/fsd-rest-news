{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module RestNews
    ( runWarpWithLogger
    ) where

import Control.Exception (bracket_)
import Control.Monad (void)
import Data.ByteString.Lazy.UTF8 (fromString)
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

selectTest :: Session Int64
selectTest = Session.statement ()
    [TH.singletonStatement|
        select ((2 :: int8) + (2 :: int8)) :: int8
        |]

dbCall :: IO ()
dbCall = let {
    connectionSettings = Connection.settings "localhost" 5432 "rest-news-user" "rest" "rest-news-db";
} in do
    --connectionAcquired <- Connection.acquire connectionSettings
    --(print $ case connectionAcquired of
    --    Right connection -> Session.run selectTest connection
    --    Left connectionError -> pure $ connectionError)
    Right connection <- Connection.acquire connectionSettings
    results <- Session.run selectTest connection
    print results

restAPI :: Application;
restAPI request respond = bracket_
    (debugM "rest-news" "Allocating scarce resource")
    (debugM "rest-news" "Cleaning up")
    (respond $ responseLBS H.status200 [] (fromString $ show request))

runWarp :: IO ()
runWarp = let {
    port = 8081 :: Port;
} in run port restAPI

runWarpWithLogger :: IO ()
runWarpWithLogger = runWarp
