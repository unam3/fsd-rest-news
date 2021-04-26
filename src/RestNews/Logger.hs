module RestNews.Logger
    ( Config (..)
    , Handle (..)
    , withLogger
    ) where

import Prelude hiding (init)
import System.Log.Logger (Priority)


data Config a = Config {
    cPriorityLevelToLog :: Priority,
    cInit :: Priority -> IO a,
    cDebug :: String -> IO a,
    cError :: String -> IO a
}

data Handle a = Handle {
    hDebug :: String -> IO a,
    hError :: String -> IO a
}


withLogger :: Config a -> (Handle a -> IO b) -> IO b
withLogger config f = do
    let handle = Handle (cDebug config) (cError config)
    _ <- (cInit config) (cPriorityLevelToLog config)
    f handle 
