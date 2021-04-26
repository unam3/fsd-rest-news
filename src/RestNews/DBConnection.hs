module RestNews.DBConnection
    ( Config (..)
    , Handle (..)
    , withDBConnection
    ) where

import Hasql.Connection (Connection, ConnectionError)
import Network.Wai (Application)


newtype Config = Config {
    cAcquiredConnection :: IO (Either ConnectionError Connection)
}

newtype Handle = Handle {
    hAcquiredConnection :: IO (Either ConnectionError Connection)
}


withDBConnection :: Config -> (Handle -> Application) -> Application
withDBConnection config f = f . Handle $ cAcquiredConnection config
