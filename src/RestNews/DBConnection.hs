module RestNews.DBConnection
    ( Config (..)
    , Handle (..)
    , withDBConnection
    ) where

import Hasql.Connection (Connection, ConnectionError)


newtype Config = Config {
    cAcquiredConnection :: IO (Either ConnectionError Connection)
}

newtype Handle = Handle {
    hAcquiredConnection :: IO (Either ConnectionError Connection)
}


withDBConnection :: Config -> (Handle -> IO ()) -> IO ()
withDBConnection config f = f . Handle $ cAcquiredConnection config
