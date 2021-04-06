module RestNews.Middleware.Sessions
    ( Config (..)
    , Handle (..)
    , withSessions
    ) where

import Network.Wai (Middleware, Request)
import Prelude hiding (init)


data Config a = Config {
    cWithSession :: Middleware,
    cMaybeSessionMethods :: Request -> Maybe (String -> IO (Maybe String), String -> String -> IO ()),
    cClearSession :: Request -> IO ()
}

data Handle a = Handle {
    hWithSession :: Middleware,
    hMaybeSessionMethods :: Request -> Maybe (String -> IO (Maybe String), String -> String -> IO ()),
    hClearSession :: Request -> IO ()
}


withSessions :: Config a -> (Handle a -> IO a) -> IO a
withSessions config f = f $ Handle (cWithSession config) (cMaybeSessionMethods config) (cClearSession config)
