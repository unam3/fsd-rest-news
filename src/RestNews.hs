{-# LANGUAGE OverloadedStrings #-}

module RestNews
    ( runWarpWithLogger
    ) where

import Control.Exception (bracket_)
import qualified Network.HTTP.Types as H
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (Port, run)
import System.Log.Logger (Priority (DEBUG, ERROR), debugM, setLevel, traplogging, updateGlobalLogger)

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
runWarpWithLogger = traplogging "rest-news" ERROR "Shutdown due to" $
    (updateGlobalLogger "rest-news" (setLevel DEBUG) >> runWarp)
