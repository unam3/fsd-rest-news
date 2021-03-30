module RestNews.Logger
    ( Config (..)
    , Handle (..)
    , withLogger
    ) where

import Prelude hiding (init)
import System.Log.Logger (Priority)


data Config = Config {
    cPriorityLevelToLog :: Priority,
    cInit :: Priority -> IO (),
    cDebug :: String -> IO (),
    cError :: String -> IO ()
}

data Handle = Handle {
    hDebug :: String -> IO (),
    hError :: String -> IO ()
}


withLogger :: Config -> (Handle -> IO ()) -> IO ()
withLogger config f = do
    let handle = Handle (cDebug config) (cError config)
    (cInit config) (cPriorityLevelToLog config)
    f handle 
