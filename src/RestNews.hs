{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( runWarp
    ) where

import Control.Exception (bracket_)
import qualified Network.HTTP.Types as H
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (Port, run)

restAPI :: Application;
restAPI request respond = bracket_
    (putStrLn "Allocating scarce resource")
    (putStrLn "Cleaning up")
    (respond $ responseLBS H.status200 [] "Hello World")

runWarp :: IO ()
--someFunc = putStrLn "someFunc"
runWarp = let {
    port = 8081 :: Port;
} in run port restAPI
