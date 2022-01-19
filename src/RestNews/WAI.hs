module RestNews.WAI
    ( Config(..)
    , Handle(..)
    , withWAI
    ) where

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Network.HTTP.Types.URI (Query)
import Network.HTTP.Types.Method (Method)
import Network.Wai (Application, Request)


data Config a = Config {
    cRequestMethod :: Request -> Method,
    cQueryString :: Request -> Query,
    cPathInfo :: Request -> [Text],
    cStrictRequestBody :: Request -> IO ByteString
}

data Handle a = Handle {
    hRequestMethod :: Request -> Method,
    hQueryString :: Request -> Query,
    hPathInfo :: Request -> [Text],
    hStrictRequestBody :: Request -> IO ByteString
}

withWAI :: Config a -> (Handle a -> Application) -> Application
withWAI config f =
    f
        $ Handle
            (cRequestMethod config)
            (cQueryString config)
            (cPathInfo config)
            (cStrictRequestBody config)
