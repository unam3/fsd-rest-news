module RestNews.Middleware.Sessions
    ( Config (..)
    , Handle (..)
    , withSessions
    ) where

import Network.Wai (Middleware, Request)
import Prelude hiding (init)


data Config = Config {
    cWithSession :: Middleware,
    cMaybeSessionMethods :: Request -> Maybe (String -> IO (Maybe String), String -> String -> IO ()),
    --(Request -> m ())
    cClearSession :: Request -> IO ()
}

data Handle = Handle {
    hWithSession :: Middleware,
    hMaybeSessionMethods :: Request -> Maybe (String -> IO (Maybe String),String -> String -> IO ()),
    hClearSession :: Request -> IO ()
}


withSessions :: Config -> (Handle -> IO ()) -> IO ()
withSessions config f = f $ Handle (cWithSession config) (cMaybeSessionMethods config) (cClearSession config)
