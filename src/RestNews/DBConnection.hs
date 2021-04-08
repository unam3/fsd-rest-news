module RestNews.DBConnection
    ( Config (..)
    , Handle (..)
    , withDBConnection
    ) where

import Hasql.Connection (Connection, ConnectionError)


newtype Config a = Config {
    cAcquiredConnection :: IO (Either ConnectionError Connection)
}

newtype Handle a = Handle {
    hAcquiredConnection :: IO (Either ConnectionError Connection)
}


withDBConnection :: Config a -> (Handle a -> IO a) -> IO a
withDBConnection config f = f . Handle $ cAcquiredConnection config
